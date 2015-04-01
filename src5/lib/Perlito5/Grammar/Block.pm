
package Perlito5::Grammar::Block;

use Perlito5::Grammar::Expression;
use strict;

sub eval_begin_block {
    # execute "eval" on this block,
    # without access to compile-time lexical variables.
    # compile-time globals are still a problem.
    local $@;
    my $code = "package $Perlito5::PKG_NAME;\n"
             . $_[0];
    eval $code;
    # say $code;
    die "Error in BEGIN block: " . $@
        if $@;
}

our %Named_block = (
    BEGIN     => 1,
    UNITCHECK => 1,
    CHECK     => 1,
    INIT      => 1,
    END       => 1,
);

sub term_block {
    my $str = $_[0];
    my $pos = $_[1];

    my $p = $pos;
    my $block_name;
    my $m_name = Perlito5::Grammar::ident( $str, $p );
    if ($m_name) {
        $p = $m_name->{to};
        $block_name = Perlito5::Match::flat($m_name);
    }

    my $ws = Perlito5::Grammar::Space::ws( $str, $p );
    if ( $ws ) {
        $p = $ws->{to};
    }

    if ( substr($str, $p, 1) eq '{' ) {
        # do we recognize a bare block in this position?
        # warn "maybe bareblock at $p";
        my $m = Perlito5::Grammar::Expression::term_curly($str, $p);
        if ($m) {
            my $block_start = $p;
            $p = $m->{to};
            $ws = Perlito5::Grammar::Space::ws( $str, $p );
            if ( $ws ) {
                $p = $ws->{to};
            }
            my $continue = Perlito5::AST::Lit::Block->new(stmts => [] );
            my $has_continue = 0;
            if ( !$block_name && substr($str, $p, 8) eq 'continue' ) {
                # anonymous blocks can have a 'continue' block
                $p += 8;
                $ws = Perlito5::Grammar::Space::ws( $str, $p );
                if ( $ws ) {
                    $p = $ws->{to};
                }
                my $cont = Perlito5::Grammar::Expression::term_curly($str, $p);
                die "syntax error" unless $cont;
                # warn "continue!";

                $continue->{stmts} = $cont->{capture}[2];
                $has_continue = 1;
                $m->{to} = $cont->{to};
            }

            my $v = Perlito5::Match::flat($m);

            # TODO - this is not recognized as a statement: { 123 => 4;}
            # TODO - this is not recognized as a syntax error: { 123 => 4 }{2}

            $v = Perlito5::AST::Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
            $v = Perlito5::Grammar::Expression::block_or_hash($v)
                unless $has_continue || $block_name;

            if ( ref($v) eq 'Perlito5::AST::Lit::Block' ) {
                if ($block_name eq 'BEGIN') {
                    # say "BEGIN $block_start ", $m->{to}, "[", substr($str, $block_start, $m->{to} - $block_start), "]";
                    # local $Perlito5::PKG_NAME = $Perlito5::PKG_NAME;  # BUG - this doesn't work
                    local $Perlito5::PHASE = 'BEGIN';
                    eval_begin_block( substr($str, $block_start, $m->{to} - $block_start) );
                    $m->{capture} = 
                        Perlito5::AST::Apply->new(
                            code => 'undef',
                            namespace => '',
                            arguments => []
                        );
                }
                else {
                    $v->{name} = $block_name;
                    $m->{capture} = $v;
                    $m->{capture}{continue} = $continue;
                }
                return $m;
            }
        }
    }

    return 0;
}

token named_sub_def {
    <Perlito5::Grammar::optional_namespace_before_ident> <Perlito5::Grammar::ident>
    <Perlito5::Grammar::Block::prototype_> <.Perlito5::Grammar::Space::opt_ws>
    <Perlito5::Grammar::Attribute::opt_attribute> <.Perlito5::Grammar::Space::opt_ws>
    [
        \{
        <.Perlito5::Grammar::Space::opt_ws>
        <Perlito5::Grammar::exp_stmts>
        <.Perlito5::Grammar::Space::opt_ws>
        [   \}     | { die 'Missing right curly or square bracket in sub \'', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::ident"}), '\'' } ]
        {
            $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::exp_stmts"});
        }
    |
        <.Perlito5::Grammar::Statement::statement_parse>
        {
            die 'Illegal declaration of subroutine \'', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::ident"}), '\''
        }
    |
        {
            # subroutine predeclaration - there is no block
            $MATCH->{_tmp} = undef;
        }
    ]
    {
        my $name = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::ident"});
        my $sig  = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Block::prototype_"});
        $sig = undef if $sig eq '*undef*';

        my $attributes = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Attribute::opt_attribute"});
        my ($proto) = grep { $_->[0] eq 'prototype' } @$attributes;
        if ($proto) {
            $attributes = [grep { $_->[0] ne 'prototype' } @$attributes];
            $sig = $proto->[1];
        }

        my $namespace = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::optional_namespace_before_ident"});
        if ( $name ) {
            # say "sub $Perlito5::PKG_NAME :: $name ( $sig )";
            if (!$namespace) {
                #  perl -MO=Deparse -e ' package X; sub _ { 123 } '  # sub main::_
                $namespace = $name eq '_'
                            ? 'main'
                            : $Perlito5::PKG_NAME;
            }

            my $full_name = "${namespace}::$name";

            # TODO - check if the previous definition was a predeclaration
            # warn "Subroutine $full_name redefined"
            #     if exists $Perlito5::PROTO->{$full_name};

            $Perlito5::PROTO->{$full_name} = $sig;  # TODO - cleanup - replace $PROTO with prototype()
            # if (!exists(&{$full_name})) {
            #     # make sure the prototype exists at compile-time
            #     my $sub = defined($sig)
            #             ? eval "sub ($sig) { }"
            #             : eval "sub { }";
            #     *{$full_name} = $sub;
            # }
        }
        $MATCH->{capture} = Perlito5::AST::Sub->new(
            name       => $name, 
            namespace  => $namespace,
            sig        => $sig, 
            block      => $MATCH->{_tmp},
            attributes => $attributes,
        ) 
    }
};

sub named_sub {
    my $str = $_[0];
    my $pos = $_[1];

    return
        unless substr($str, $pos, 3) eq 'sub';
    my $ws = Perlito5::Grammar::Space::ws( $str, $pos + 3 );
    return
        unless $ws;
    my $p = $ws->{to};

    my $m_name = Perlito5::Grammar::ident( $str, $p );
    return
        unless $m_name;

    my $block_name = Perlito5::Match::flat($m_name);
    if (exists $Named_block{$block_name}) {
        return Perlito5::Grammar::Block::term_block($str, $p);
    }
    return Perlito5::Grammar::Block::named_sub_def($str, $p);
}

token term_anon_sub {
    'sub' <.Perlito5::Grammar::Space::opt_ws> <Perlito5::Grammar::Block::anon_sub_def>
                { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Block::anon_sub_def"})     ] }
};

token term_do {
    # Note: this is do-block; do-string is parsed as a normal subroutine
    'do' <.Perlito5::Grammar::Space::ws> <before '{'> <Perlito5::Grammar::Statement::statement_parse>
                { $MATCH->{capture} = [ 'term', Perlito5::AST::Do->new( block => Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Statement::statement_parse'}) ) ] }
};

token args_sig {
    [ ';' | '\\' | '[' | ']' | '*' | '+' | '@' | '%' | '$' | '&' ]*
};

token prototype_ {
    |   <.Perlito5::Grammar::Space::opt_ws> \( <.Perlito5::Grammar::Space::opt_ws>  <args_sig>  <.Perlito5::Grammar::Space::opt_ws>  \)
        { $MATCH->{capture} = "" . Perlito5::Match::flat($MATCH->{args_sig}) }
    |   { $MATCH->{capture} = '*undef*' }   # default signature
};

token anon_sub_def {
    <prototype_> <.Perlito5::Grammar::Space::opt_ws> 
    <Perlito5::Grammar::Attribute::opt_attribute> <.Perlito5::Grammar::Space::opt_ws>
    \{ 
        <.Perlito5::Grammar::Space::opt_ws> 
        <Perlito5::Grammar::exp_stmts> 
        <.Perlito5::Grammar::Space::opt_ws>
    [   \}     | { die 'Missing right curly or square bracket in anon sub' } ]
    {
        my $sig  = Perlito5::Match::flat($MATCH->{prototype_});
        $sig = undef if $sig eq '*undef*';

        my $attributes = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Attribute::opt_attribute"});
        my ($proto) = grep { $_->[0] eq 'prototype' } @$attributes;
        if ($proto) {
            $attributes = [grep { $_->[0] ne 'prototype' } @$attributes];
            $sig = $proto->[1];
        }

        $MATCH->{capture} = Perlito5::AST::Sub->new(
            name  => undef, 
            namespace => undef,
            sig   => $sig, 
            block => Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::exp_stmts'}),
            attributes => $attributes,
        ) 
    }
};


Perlito5::Grammar::Precedence::add_term( 'do'    => \&term_do );
Perlito5::Grammar::Precedence::add_term( 'sub'   => \&term_anon_sub );

Perlito5::Grammar::Statement::add_statement( '{'     => \&term_block );
Perlito5::Grammar::Statement::add_statement( 'sub'   => \&named_sub );
Perlito5::Grammar::Statement::add_statement( $_      => \&term_block )
    for keys %Named_block;


1;

=begin

=head1 NAME

Perlito5::Grammar::Block - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_block($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


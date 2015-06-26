
package Perlito5::Grammar::Block;

use Perlito5::Grammar::Expression;
use strict;

our %Named_block = (
    BEGIN     => 1,
    UNITCHECK => 1,
    CHECK     => 1,
    INIT      => 1,
    END       => 1,
);

our %Special_var = (
    ARGV => 1,
    INC  => 1,
    ENV  => 1,
    _    => 1,
);

sub lookup_variable {
    # search for a variable declaration in the compile-time scope
    my $var = shift;

    return $var if $var->{namespace};       # global variable
    return $var if $var->{_decl};           # predeclared variable
    return $var if $var->{sigil} eq '&';    # &sub - TODO

    my $c = substr($var->{name}, 0, 1);
    if ( $Special_var{ $var->{name} } || $c lt 'A' || ($c gt 'Z' && $c lt 'a') || $c gt 'z') {
        # special variable
        return $var;
    }

    my $scope = shift() // $Perlito5::BASE_SCOPE;
    my $block = $scope->{block};
    if ( @$block && ref($block->[-1]) eq 'HASH' && $block->[-1]{block} ) {
        # lookup in the inner scope first
        my $look = lookup_variable($var, $block->[-1]);
        return $look if $look;
    }
    for my $item (reverse @$block) {
        if (ref($item) eq 'Perlito5::AST::Var' && $item->{_decl} && $item->{name} eq $var->{name}) {
            my $sigil = $var->{_real_sigil} || $var->{sigil};
            # TODO - namespace
            # TODO - $a[10]  $#a  ${"a"}
            # TODO - check "strict"
            if ($sigil eq $item->{sigil}) {
                # print "found name: $item->{sigil} $item->{name} decl: $item->{_decl}\n";
                return $item;
            }
        }
    }
    # print "not found\n";
    return;
}

sub block {
    my $str = $_[0];
    my $pos = $_[1];
    my $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( substr($str, $pos, 1) ne '{' ) {
        return
    }
    $pos++;

    # when parsing a command like "for my $x ..." register the loop variable
    # before entering the block, so that it can be seen immediately
    Perlito5::Grammar::Statement::check_variable_declarations();
    my $new_scope = { block => [] };
    push @{ $Perlito5::SCOPE->{block} }, $new_scope;   # start new lexical scope
    local $Perlito5::SCOPE = $new_scope;

    $m = Perlito5::Grammar::exp_stmts($str, $pos);
    if (!$m) {
        die "syntax error";
    }
    $pos = $m->{to};
    my $capture = Perlito5::Match::flat($m);
    $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( substr($str, $pos, 1) ne '}' ) {
        die "syntax error";
    }
    $m->{to} = $pos + 1;
    $m->{capture} = Perlito5::AST::Block->new( stmts => $capture, sig => undef );
    # end of lexical scope
    return $m;
}

sub eval_begin_block {
    # execute "eval" on this block,
    # without access to compile-time lexical variables.
    # compile-time globals are still a problem.
    local $@;
    my $code = "package $Perlito5::PKG_NAME;\n"
             . $_[0];
    # say $code;
    eval "{ $code }; 1"
    or die "Error in BEGIN block: " . $@;
}

token opt_continue_block {
        <.Perlito5::Grammar::Space::opt_ws> 'continue' <block>
        {
            $MATCH->{capture} = Perlito5::Match::flat($MATCH->{block});
            $MATCH->{capture}{is_continue} = 1;
        }
    |
        {
            $MATCH->{capture} = Perlito5::AST::Block->new( stmts => [], sig => undef )
        }
};

sub anon_block {
    my $str = $_[0];
    my $pos = $_[1];

    my $p = $pos;
    my $m = Perlito5::Grammar::block( $str, $p );
    return if !$m;
    $p = $m->{to};
    my $block = Perlito5::Match::flat($m);
   
    # anonymous blocks can have a 'continue' block
    $m = Perlito5::Grammar::opt_continue_block( $str, $p );
    $p = $m->{to};
    my $continue = Perlito5::Match::flat($m);

    my $v = $block;

    # TODO - this is not recognized as a statement: { 123 => 4;}
    # TODO - this is not recognized as a syntax error: { 123 => 4 }{2}
    $v = Perlito5::Grammar::Expression::block_or_hash($v)
        unless $continue->{is_continue};
    $m->{capture} = $v;
    if ( $continue->{is_continue} ) {
        $m->{capture}{continue} = $continue;
    }
    return $m;
}

sub special_named_block {
    my $str = $_[0];
    my $pos = $_[1];

    my $p = $pos;
    my $block_name;
    my $m_name = Perlito5::Grammar::ident( $str, $p );
    return if !$m_name;
    $p = $m_name->{to};
    $block_name = Perlito5::Match::flat($m_name);

    my $ws = Perlito5::Grammar::Space::opt_ws( $str, $p );
    $p = $ws->{to};

    my $block_start = $p;
    my $m = Perlito5::Grammar::block( $str, $p );
    return if !$m;
    $p = $m->{to};
    my $block = Perlito5::Match::flat($m);
 
    my $compile_block = $Perlito5::SCOPE->{block}[-1];
    $compile_block->{type} = 'sub';
    $compile_block->{name} = $block_name;
  
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
        $m->{capture} = $block;
        $m->{capture}{name} = $block_name;
    }
    return $m;
}

token named_sub_def {
    <Perlito5::Grammar::optional_namespace_before_ident> <Perlito5::Grammar::ident>
    <Perlito5::Grammar::Block::prototype_> <.Perlito5::Grammar::Space::opt_ws>
    <Perlito5::Grammar::Attribute::opt_attribute> <.Perlito5::Grammar::Space::opt_ws>
    [
        <Perlito5::Grammar::block>
        {
            $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::block"});
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

            if ($MATCH->{_tmp}) {
                my $block = $Perlito5::SCOPE->{block}[-1];
                $block->{type} = 'sub';
                $block->{name} = $full_name;
            }
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
        return Perlito5::Grammar::Block::special_named_block($str, $p);
    }
    return Perlito5::Grammar::Block::named_sub_def($str, $p);
}

token term_anon_sub {
    'sub' <.Perlito5::Grammar::Space::opt_ws> <Perlito5::Grammar::Block::anon_sub_def>
                { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Block::anon_sub_def"})     ] }
};

token term_do {
    # Note: this is do-block; do-string is parsed as a normal subroutine
    'do' <Perlito5::Grammar::block>
        { $MATCH->{capture} = [ 'term', Perlito5::AST::Apply->new(
                                    code  => 'do',
                                    arguments => [ Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::block'}) ]
                                )
                              ]
        }
};

token args_sig {
    [ ';' | '\\' | '[' | ']' | '*' | '+' | '@' | '%' | '$' | '&' ]*
};

token prototype_ {
    |   <.Perlito5::Grammar::Space::opt_ws> \( <.Perlito5::Grammar::Space::opt_ws>  '_'  <.Perlito5::Grammar::Space::opt_ws>  \)
        { $MATCH->{capture} = "_" }
    |   <.Perlito5::Grammar::Space::opt_ws> \( <.Perlito5::Grammar::Space::opt_ws>  <args_sig>  <.Perlito5::Grammar::Space::opt_ws>  \)
        { $MATCH->{capture} = "" . Perlito5::Match::flat($MATCH->{args_sig}) }
    |   { $MATCH->{capture} = '*undef*' }   # default signature
};

token anon_sub_def {
    <prototype_> <.Perlito5::Grammar::Space::opt_ws> 
    <Perlito5::Grammar::Attribute::opt_attribute>
    <Perlito5::Grammar::block>
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
            block => Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::block'}),
            attributes => $attributes,
        ) 
    }
};


Perlito5::Grammar::Precedence::add_term( 'do'    => \&term_do );
Perlito5::Grammar::Precedence::add_term( 'sub'   => \&term_anon_sub );

Perlito5::Grammar::Statement::add_statement( '{'     => \&anon_block );
Perlito5::Grammar::Statement::add_statement( 'sub'   => \&named_sub );
Perlito5::Grammar::Statement::add_statement( $_      => \&special_named_block )
    for keys %Named_block;


1;

=begin

=head1 NAME

Perlito5::Grammar::Block - Parser and AST generator for Perlito

=head1 SYNOPSIS

    anon_block($str)

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


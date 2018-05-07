
package Perlito5::Grammar::Block;

use Perlito5::Grammar::Expression;
use Perlito5::Grammar::Scope;
use Perlito5::AST::BeginScratchpad;
use Perlito5::AST::Captures;
use strict;

our %Named_block = (
    BEGIN     => 1,
    UNITCHECK => 1,
    CHECK     => 1,
    INIT      => 1,
    END       => 1,
    AUTOLOAD  => 1,
    DESTROY   => 1,
);

sub block {
    my $str = $_[0];
    my $pos = $_[1];
    my $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( $str->[$pos] ne '{' ) {
        return
    }
    $pos++;

    # when parsing a command like "for my $x ..." register the loop variable
    # before entering the block, so that it can be seen immediately
    Perlito5::Grammar::Scope::check_variable_declarations();
    Perlito5::Grammar::Scope::create_new_compile_time_scope();

    $m = Perlito5::Grammar::exp_stmts($str, $pos);
    if (!$m) {
        Perlito5::Compiler::error "syntax error";
    }
    $pos = $m->{to};
    my $capture = Perlito5::Match::flat($m);
    $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( $str->[$pos] ne '}' ) {
        Perlito5::Compiler::error "syntax error";
    }
    $m->{to} = $pos + 1;
    $m->{capture} = Perlito5::AST::Block->new( stmts => $capture, sig => undef );
    # end of lexical scope
    Perlito5::Grammar::Scope::end_compile_time_scope();
    return $m;
}

sub closure_block {
    my $str = $_[0];
    my $pos = $_[1];
    my $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( $str->[$pos] ne '{' ) {
        return
    }
    $pos++;

    # when parsing a command like "for my $x ..." register the loop variable
    # before entering the block, so that it can be seen immediately
    Perlito5::Grammar::Scope::check_variable_declarations();
    Perlito5::Grammar::Scope::create_new_compile_time_scope();
    local $Perlito5::CLOSURE_SCOPE = $#Perlito5::BASE_SCOPE;  # this is the only diff from plain <block>

    $m = Perlito5::Grammar::exp_stmts($str, $pos);
    if (!$m) {
        Perlito5::Compiler::error "syntax error";
    }
    $pos = $m->{to};
    my $capture = Perlito5::Match::flat($m);
    $m = Perlito5::Grammar::Space::opt_ws($str, $pos);
    $pos = $m->{to};
    if ( $str->[$pos] ne '}' ) {
        Perlito5::Compiler::error "syntax error";
    }

    $m->{to} = $pos + 1;
    $m->{capture} = Perlito5::AST::Block->new( stmts => $capture, sig => undef );
    # end of lexical scope
    Perlito5::Grammar::Scope::end_compile_time_scope();
    return $m;
}

sub eval_end_block {
    # execute "eval" on this block,
    # without access to compile-time lexical variables.
    # compile-time globals are still a problem.
    my ($block, $phase) = @_;

    $block = Perlito5::AST::Block->new(
        stmts => [
            Perlito5::AST::Sub->new(
                'attributes' => [],
                'block'      => $block,
                'name'       => undef,
                'namespace'  => $Perlito5::PKG_NAME,
                'sig'        => undef,
                'pos'        => Perlito5::Compiler::compiler_pos(),
            )
        ]
    );
    return Perlito5::Grammar::Block::eval_begin_block($block, 'BEGIN');  
}

sub eval_begin_block {
    # execute "eval" on this block,
    # without access to compile-time lexical variables.
    # compile-time globals are still a problem.
    my $block = shift;
    local ${^GLOBAL_PHASE};
    Perlito5::set_global_phase("BEGIN");

    # get list of captured variables, including inner blocks
    my @captured = $block->get_captures();
    my %dont_capture = map { $_->{dont} ? ( $_->{dont} => 1 ) : () } @captured;
    my %capture = map { $_->{dont} ? ()
                      : $dont_capture{ $_->{_id} } ? ()
                      : ($_->{_decl} eq 'local' || $_->{_decl} eq 'global' || $_->{_decl} eq 'our' || $_->{_decl} eq '') ? ()
                      : ( $_->{_id} => $_ )
                      } @captured;

    # print STDERR "CAPTURES ", Data::Dumper::Dumper(\%capture);

    # %capture == (
    #     '100' => ...,
    #     '101' => ...,,
    # )
    %Perlito5::BEGIN_SCRATCHPAD = ( %Perlito5::BEGIN_SCRATCHPAD, %capture );

    # use lexicals from BEGIN scratchpad
    $block = $block->emit_begin_scratchpad();

    # emit_compile_time() adds instrumentation to inspect captured variables
    $block = $block->emit_compile_time();

    local $@;
    my $result = Perlito5::eval_ast($block);
    if ($@) {
        Perlito5::Compiler::error "Error in BEGIN block: " . $@;
    }

    # "use MODULE" wants a true return value
    return $result;
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
    local $Perlito5::BLOCK_HAS_SEMICOLON;
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
        if !$continue->{is_continue}
        && !$Perlito5::BLOCK_HAS_SEMICOLON;
    $m->{capture} = $v;
    if ( $continue->{is_continue} ) {
        $m->{capture}{continue} = $continue;
    }
    return $m;
}

sub ast_nop {
    Perlito5::AST::Apply->new(
        code => 'nop',
        namespace => 'Perlito5',
        arguments => []
    );
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
    my $m = Perlito5::Grammar::Block::closure_block( $str, $p );
    return if !$m;
    $p = $m->{to};
    my $block = Perlito5::Match::flat($m);
 
    if ($block_name eq 'INIT') {
        push @Perlito5::INIT_BLOCK, eval_end_block( $block, 'INIT' );
        $m->{capture} = ast_nop();
    }
    elsif ($block_name eq 'END') {
        unshift @Perlito5::END_BLOCK, eval_end_block( $block, 'END' );
        $m->{capture} = ast_nop();
    }
    elsif ($block_name eq 'CHECK') {
        unshift @Perlito5::CHECK_BLOCK, eval_end_block( $block, 'CHECK' );
        $m->{capture} = ast_nop();
    }
    elsif ($block_name eq 'UNITCHECK') {
        unshift @Perlito5::UNITCHECK_BLOCK, eval_end_block( $block, 'UNITCHECK' );
        $m->{capture} = ast_nop();
    }
    elsif ($block_name eq 'BEGIN') {
        # say "BEGIN $block_start ", $m->{to}, "[", substr($str, $block_start, $m->{to} - $block_start), "]";
        # local $Perlito5::PKG_NAME = $Perlito5::PKG_NAME;  # BUG - this doesn't work
        local $Perlito5::PHASE = 'BEGIN';
        eval_begin_block( $block );
        $m->{capture} = ast_nop();
    }
    elsif ($block_name eq 'AUTOLOAD' || $block_name eq 'DESTROY') {
        my $sub = Perlito5::AST::Sub->new(
            'attributes' => [],
            'block'      => $block,
            'name'       => $block_name,
            'namespace'  => $Perlito5::PKG_NAME,
            'sig'        => undef,
            'pos'        => Perlito5::Compiler::compiler_pos(),
        );
        # add named sub to SCOPE
        my $full_name = $sub->{namespace} . "::" . $sub->{name};
        $Perlito5::PROTO->{$full_name} = undef;
        $Perlito5::GLOBAL->{$full_name} = $sub;
        # evaluate the sub definition in a BEGIN block
        $block = Perlito5::AST::Block->new( stmts => [$sub] );
        Perlito5::Grammar::Block::eval_begin_block($block, 'BEGIN');  
        # runtime effect of subroutine declaration is "undef"
        $m->{capture} = ast_nop();
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
        <Perlito5::Grammar::Block::closure_block>
        {
            $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Block::closure_block"});
        }
    |
        <.Perlito5::Grammar::Statement::statement_parse>
        {
            Perlito5::Compiler::error 'Illegal declaration of subroutine \'', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::ident"}), '\''
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
        }
        my $sub = Perlito5::AST::Sub->new(
            name       => $name, 
            namespace  => $namespace,
            sig        => $sig, 
            block      => $MATCH->{_tmp},
            attributes => $attributes,
            pos        => Perlito5::Compiler::compiler_pos(),
        );

        if ($name && defined $sig && $sig eq '' && $sub->{block} && @{ $sub->{block}{stmts} } == 1 ) {
            my $expr = $sub->{block}{stmts}[0];
            my $ref = ref($expr);
            if (   $ref eq 'Perlito5::AST::Int'
                || $ref eq 'Perlito5::AST::Num'
                || $ref eq 'Perlito5::AST::Buf'
               )
            {
                # looks like a constant declaration

                # TODO - "Constant subroutine xx redefined"

                # print STDERR "maybe constant $namespace :: $name ($sig)\n";
                $Perlito5::CONSTANT{"${namespace}::$name"} = $expr;
            }
        }

        if ( $Perlito5::EXPAND_USE && $name ) {
            # named sub in the normal compiler (not "bootstrapping")
            my $full_name = "${namespace}::$name";

            # evaluate the sub definition in a BEGIN block
            my $block = Perlito5::AST::Block->new( stmts => [$sub] );
            Perlito5::Grammar::Block::eval_begin_block($block, 'BEGIN');  

            # add named sub to SCOPE
            $Perlito5::GLOBAL->{$full_name} = $sub;

            # runtime effect of subroutine declaration is "undef"
            $sub = ast_nop();
            $MATCH->{capture} = $sub;
        }
        else {
            # bootstrapping mode
            # the subroutine AST is directly added to the global AST
            $MATCH->{capture} = $sub;
        }
    }
};

sub named_sub {
    my $str = $_[0];
    my $pos = $_[1];

    return
        unless $str->[$pos] eq 's' && $str->[$pos+1] eq 'u' && $str->[$pos+2] eq 'b';
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
    [ ';' | \\ | '[' | ']' | '*' | '+' | '@' | '%' | '$' | '&' ]*
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
    <Perlito5::Grammar::Block::closure_block>
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
            name       => undef, 
            namespace  => undef,
            sig        => $sig, 
            block      => Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Block::closure_block'}),
            attributes => $attributes,
            pos        => Perlito5::Compiler::compiler_pos(),
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
The Pugs Team.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


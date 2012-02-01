package Perlito5::Grammar;

use Perlito5::Expression;
use Perlito5::Grammar::Regex;
use Perlito5::Grammar::Control;

token is_newline {
    | \c10 \c13?
    | \c13 \c10?
}

token not_newline {
    <!before \n> .
}

token ident {
    <!before \d > <.word>+
}

token full_ident {
    <.ident>  [ '::' <.ident> ]*
}

token namespace_before_ident {
    <.ident> <before '::'>   [ '::' <.ident> <before '::'> ]*
}
token optional_namespace_before_ident {
    | <namespace_before_ident> '::'
        { $MATCH->{"capture"} = $MATCH->{"namespace_before_ident"}->flat() }
    | ''
        { $MATCH->{"capture"} = '' }
}

token pod_begin {
    |   \n '=end' \N*
    |   . \N* <.pod_begin>
}

token ws {
    [
    |   '#' \N*
    |
        [ \c10 \c13?
        | \c13 \c10?
        ]

        [
        |  '=begin'  <.pod_begin>
        |  '=for'    <.pod_begin>  # fixme
        |  ''
        ]
    |   \s
    ]+
}

token opt_ws  {  <.ws>?  }
token opt_ws2 {  <.ws>?  }
token opt_ws3 {  <.ws>?  }

token grammar {
    <full_ident> <.ws>?
    '{'
        <.ws>?
        <exp_stmts>
        <.ws>?
    '}'
    {
        $MATCH->{"capture"} = CompUnit->new(
            name        => $MATCH->{"full_ident"}->flat(),
            body        => $MATCH->{"exp_stmts"}->flat(),
        )
    }
}

token declarator {
     'my' | 'state' | 'has'
}

token exp_stmts2 { <exp_stmts> { $MATCH->{"capture"} = $MATCH->{"exp_stmts"}->flat() } }

token exp {
    <Perlito5::Expression.exp_parse>
        { $MATCH->{"capture"} = $MATCH->{"Perlito5::Expression.exp_parse"}->flat() }
}

token exp2 {
    <Perlito5::Expression.exp_parse>
        { $MATCH->{"capture"} = $MATCH->{"Perlito5::Expression.exp_parse"}->flat() }
}

token opt_ident {
    | <ident>  { $MATCH->{"capture"} = $MATCH->{"ident"}->flat() }
    | ''       { $MATCH->{"capture"} = 'postcircumfix:<( )>' }
}

token opt_type {
    |   '::'?  <full_ident>   { $MATCH->{"capture"} = $MATCH->{"full_ident"}->flat() }
    |   ''                    { $MATCH->{"capture"} = '' }
}

token var_sigil     { \$ |\% |\@ |\& }

token var_twigil    { [ \. | \! | \^ | \* ]? }

token var_name      { <full_ident> | <digit> }

token var_ident {
    <var_sigil> <var_twigil> <optional_namespace_before_ident> <var_name>
    {
        $MATCH->{"capture"} = Var->new(
            sigil       => $MATCH->{"var_sigil"}->flat(),
            twigil      => $MATCH->{"var_twigil"}->flat(),
            namespace   => $MATCH->{"optional_namespace_before_ident"}->flat(),
            name        => $MATCH->{"var_name"}->flat(),
        )
    }
}

token exponent {
    [ 'e' | 'E' ]  [ '+' | '-' | '' ]  \d+
}

token val_num {
    [   \. \d+    <.exponent>?
    |   \d+     [ <.exponent>  |   \. \d+  <.exponent>? ]
    ]
    { $MATCH->{"capture"} = Val::Num->new( num => $MATCH->flat() ) }
}

token char_any {
    .
}

token char_any_single_quote {
    <!before \' > .
    [ <!before [ \' | \\ ] > . ]*
}

token single_quoted_unescape {
    |  \\ \\  <single_quoted_unescape>
        { $MATCH->{"capture"} = "\\" . $MATCH->{"single_quoted_unescape"}->flat() }
    |  \\ \'  <single_quoted_unescape>
        { $MATCH->{"capture"} = '\'' . $MATCH->{"single_quoted_unescape"}->flat() }
    |  \\   <single_quoted_unescape>
        { $MATCH->{"capture"} = "\\" . $MATCH->{"single_quoted_unescape"}->flat() }
    |  <char_any_single_quote> <single_quoted_unescape>
        { $MATCH->{"capture"} = $MATCH->{"char_any_single_quote"}->flat() . $MATCH->{"single_quoted_unescape"}->flat() }
    |  ''
}

token char_any_double_quote {
    <!before   [ \" | \$ | \@ ] > .
    [ <!before [ \" | \$ | \@ | \\ ] > . ]*
}

token double_quoted_unescape {
    |  \\
        [  c
            [   \[ <digits> \]
                { $MATCH->{"capture"} = chr( $MATCH->{"digits"}->flat() ) }
            |  <digits>
                { $MATCH->{"capture"} = chr( $MATCH->{"digits"}->flat() ) }
            ]
        |  e
            { $MATCH->{"capture"} = chr(27) }
        |  n
            { $MATCH->{"capture"} = "\n" }
        |  t
            { $MATCH->{"capture"} = chr(9) }
        |  <char_any>
            { $MATCH->{"capture"} = $MATCH->{"char_any"}->flat() }
        ]
    |  <char_any_double_quote>
        { $MATCH->{"capture"} = $MATCH->{"char_any_double_quote"}->flat() }
}

token double_quoted_buf {
    | <before \$ >
        [ <before \$ <.ident> > <Perlito5::Expression.operator>
            { $MATCH->{"capture"} = ($MATCH->{"Perlito5::Expression.operator"}->flat())[1] }
        | \$\{ <ident> \}
            { $MATCH->{"capture"} = Var->new(
                    sigil  => '$',
                    twigil => '',
                    name   => $MATCH->{"ident"}->flat(),
                   )
            }
        | <char_any>
            { $MATCH->{"capture"} = Val::Buf->new( buf => $MATCH->{"char_any"}->flat() ) }
        ]
    | <before \@ >
        [ <before \@ <.ident> > <Perlito5::Expression.operator> 
            { $MATCH->{"capture"} = Apply->new(
                    namespace => '',
                    code      => 'join',
                    arguments => [ 
                        Val::Buf->new( buf => ' ' ), 
                        ($MATCH->{"Perlito5::Expression.operator"}->flat())[1] 
                    ],
                )
            }
        | \@\{ <exp_stmts> \}
            { $MATCH->{"capture"} = Apply->new(
                    namespace => '',
                    code      => 'join',
                    arguments => [ 
                        Val::Buf->new( buf => ' ' ), 
                        ($MATCH->{"exp_stmts"}->flat())[0] 
                    ],
                )
            }
        | <char_any>
            { $MATCH->{"capture"} = Val::Buf->new( buf => $MATCH->{"char_any"}->flat() ) }
        ]
    | <double_quoted_unescape>
        { $MATCH->{"capture"} = Val::Buf->new( buf => $MATCH->{"double_quoted_unescape"}->flat() ) }
}

token val_buf {
    | \" <double_quoted_buf>*      \"
        {
            my $args = $MATCH->{"double_quoted_buf"};
            if (!$args) {
                $MATCH->{"capture"} = Val::Buf->new( buf => '' )
            }
            else {
                $MATCH->{"capture"} = Apply->new(
                    namespace => '',
                    code => 'list:<.>',
                    arguments => [ map( $_->capture, @{$MATCH->{"double_quoted_buf"}} ) ],
                )
            }
        }
    | \' <single_quoted_unescape>  \'
        { $MATCH->{"capture"} = Val::Buf->new( buf => $MATCH->{"single_quoted_unescape"}->flat() ) }
}

token digits {
    \d+
}

token val_int {
    \d+
    { $MATCH->{"capture"} = Val::Int->new( int => $MATCH->flat() ) }
}

token exp_stmts {
    <Perlito5::Expression.delimited_statement>*
    { 
        $MATCH->{"capture"} = [ map( $_->capture, @{ $MATCH->{"Perlito5::Expression.delimited_statement"} } ) ]
    }
}

token opt_name {  <ident>?  }

token var_invocant {
    |  <var_ident> \:    { $MATCH->{"capture"} = $MATCH->{"var_ident"}->flat() }
    |  { $MATCH->{"capture"} = Var->new(
            sigil  => '$',
            twigil => '',
            name   => 'self',
         )
       }
}

token args_sig {
    <var_invocant>
    <.opt_ws>
    # TODO - Perlito5::Expression.list_parse / exp_mapping == positional / named
    <Perlito5::Expression.list_parse>
    {
        # say ' invocant: ', ($MATCH->{"var_invocant"}->flat()).perl;
        # say ' positional: ', ($MATCH->{""}->flat()).perl;
        $MATCH->{"capture"} = Sig->new(
            invocant    => $MATCH->{"var_invocant"}->flat(),
            positional  => Perlito5::Expression::expand_list(($MATCH->{"Perlito5::Expression.list_parse"}->flat()){'exp'}),
            named       => { } );
    }
}

token method_sig {
    |   <.opt_ws> \( <.opt_ws>  <args_sig>  <.opt_ws>  \)
        { $MATCH->{"capture"} = $MATCH->{"args_sig"}->flat() }
    |   { $MATCH->{"capture"} = Sig->new(
            invocant => Var->new(
                sigil  => '$',
                twigil => '',
                name   => 'self' ),
            positional => [ ],
            named => { } ) }
}

token sub_def {
    <opt_name>  <.opt_ws>
    <method_sig>
    <.opt_ws> \{ <.opt_ws>
          <exp_stmts> <.opt_ws>
    [   \}     | { die 'Syntax Error in sub \'', $MATCH->{"opt_name"}->flat(), '\''; } ]
    { $MATCH->{"capture"} = Sub->new( name => $MATCH->{"opt_name"}->flat(), sig => $MATCH->{"method_sig"}->flat(), block => $MATCH->{"exp_stmts"}->flat() ) }
}

token token {
    <opt_name>  <.opt_ws> \{
        <Perlito5::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', ($MATCH->{"Perlito5::Grammar::Regex.rule"}->flat())->perl;
        my $source = $MATCH->{"opt_name"}->flat()
            . '{ ' .
                'my $grammar = $_[0]; ' .
                'my $str     = $_[1]; ' .
                'my $pos     = $_[2]; ' .
                'my $MATCH = Perlito5::Match->new( str => $str, from => $pos, to => $pos, bool => 1 ); ' .
                '$MATCH->{"bool"} = ( ' .
                    ($MATCH->{"Perlito5::Grammar::Regex.rule"}->flat())->emit_perl5() .
                '); ' .
                '$MATCH; ' 
            . '}';
        #say 'Intermediate code: ', $source;
        my $ast = Perlito5::Grammar->sub_def( $source, 0 );
        # say 'Intermediate ast: ', $ast->flat;
        $MATCH->{"capture"} = $ast->flat();
    }
}

=begin

=head1 NAME

Perlito5::Grammar - Grammar for Perlito

=head1 SYNOPSIS

    my $match = $source.parse;
    $match->flat();    # generated Perlito AST

=head1 DESCRIPTION

This module generates a syntax tree for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2010, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

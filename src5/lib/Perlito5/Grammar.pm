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
        { make '' . $<namespace_before_ident> }
    | ''
        { make '' }
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
        make CompUnit->new(
            name        => $<full_ident>->flat(),
            body        => $<exp_stmts>->flat(),
        )
    }
}

token package_body {
    <full_ident> <.ws>?
        <exp_stmts_no_package>
    {
        make CompUnit->new(
            name        => $<full_ident>->flat(),
            body        => $<exp_stmts_no_package>->flat(),
        )
    }
}

token declarator {
     'my' | 'state' | 'has'
}

token exp_stmts2 { <exp_stmts> { make $<exp_stmts>->flat() } }

token exp {
    <Perlito5::Expression.exp_parse>
        { make $<Perlito5::Expression.exp_parse>->flat() }
}

token exp2 {
    <Perlito5::Expression.exp_parse>
        { make $<Perlito5::Expression.exp_parse>->flat() }
}

token opt_ident {
    | <ident>  { make $<ident>->flat() }
    | ''       { make 'postcircumfix:<( )>' }
}

token opt_type {
    |   '::'?  <full_ident>   { make $<full_ident>->flat() }
    |   ''                    { make '' }
}

token var_sigil     { \$ |\% |\@ |\& }

token var_twigil    { [ \. | \! | \^ | \* ]? }

token var_name      { <full_ident> | <digit> }

token var_ident {
    <var_sigil> <var_twigil> <optional_namespace_before_ident> <var_name>
    {
        make Var->new(
            sigil       => '' . $<var_sigil>,
            twigil      => '' . $<var_twigil>,
            namespace   => $<optional_namespace_before_ident>->flat(),
            name        => '' . $<var_name>,
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
    { make Val::Num->new( num => '' . $MATCH ) }
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
        { make "\\" . $<single_quoted_unescape> }
    |  \\ \'  <single_quoted_unescape>
        { make '\'' . $<single_quoted_unescape> }
    |  \\   <single_quoted_unescape>
        { make "\\" . $<single_quoted_unescape> }
    |  <char_any_single_quote> <single_quoted_unescape>
        { make $<char_any_single_quote> . $<single_quoted_unescape> }
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
                { make chr( $<digits> ) }
            |  <digits>
                { make chr( $<digits> ) }
            ]
        |  e
            { make chr(27) }
        |  n
            { make "\n" }
        |  t
            { make chr(9) }
        |  <char_any>
            { make '' . $<char_any> }
        ]
    |  <char_any_double_quote>
        { make '' . $<char_any_double_quote> }
}

token double_quoted_buf {
    | <before \$ >
        [ <before \$ <.ident> > <Perlito5::Expression.operator>
            { make ($<Perlito5::Expression.operator>->flat())[1] }
        | \$\{ <ident> \}
            { make Var->new(
                    sigil  => '$',
                    twigil => '',
                    name   => $<ident>->flat(),
                   )
            }
        | <char_any>
            { make Val::Buf->new( buf => '' . $<char_any> ) }
        ]
    | <before \@ >
        [ <before \@ <.ident> > <Perlito5::Expression.operator> 
            { make Apply->new(
                    namespace => '',
                    code      => 'join',
                    arguments => [ 
                        Val::Buf->new( buf => ' ' ), 
                        ($<Perlito5::Expression.operator>->flat())[1] 
                    ],
                )
            }
        | \@\{ <exp_stmts> \}
            { make Apply->new(
                    namespace => '',
                    code      => 'join',
                    arguments => [ 
                        Val::Buf->new( buf => ' ' ), 
                        ($<exp_stmts>->flat())[0] 
                    ],
                )
            }
        | <char_any>
            { make Val::Buf->new( buf => '' . $<char_any> ) }
        ]
    | <double_quoted_unescape>
        { make Val::Buf->new( buf => $<double_quoted_unescape>->flat() ) }
}

token val_buf {
    | \" <double_quoted_buf>*      \"
        {
            my $args = $<double_quoted_buf>;
            if (!$args) {
                make Val::Buf->new( buf => '' )
            }
            else {
                make Apply->new(
                    namespace => '',
                    code => 'list:<.>',
                    arguments => [ map( $_->capture, @{$<double_quoted_buf>} ) ],
                )
            }
        }
    | \' <single_quoted_unescape>  \'
        { make Val::Buf->new( buf => $<single_quoted_unescape>->flat() ) }
}

token digits {
    \d+
}

token val_int {
    \d+
    { make Val::Int->new( int => '' . $MATCH ) }
}

token exp_stmts {
    <Perlito5::Expression.delimited_statement>*
    { make $<Perlito5::Expression.delimited_statement>.>>capture }
}

token exp_stmts_no_package {
    <Perlito5::Expression.delimited_statement_no_package>*
    { make $<Perlito5::Expression.delimited_statement_no_package>.>>capture }
}

token opt_name {  <ident>?  }

token var_invocant {
    |  <var_ident> \:    { make $<var_ident>->flat() }
    |  { make Var->new(
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
        # say ' invocant: ', ($<var_invocant>->flat()).perl;
        # say ' positional: ', ($<>->flat()).perl;
        make Sig->new(
            invocant    => $<var_invocant>->flat(),
            positional  => Perlito5::Expression::expand_list(($<Perlito5::Expression.list_parse>->flat()){'exp'}),
            named       => { } );
    }
}

token method_sig {
    |   <.opt_ws> \( <.opt_ws>  <args_sig>  <.opt_ws>  \)
        { make $<args_sig>->flat() }
    |   { make Sig->new(
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
    [   \}     | { die 'Syntax Error in sub \'', $<opt_name>->flat(), '\''; } ]
    { make Sub->new( name => $<opt_name>->flat(), sig => $<method_sig>->flat(), block => $<exp_stmts>->flat() ) }
}

token token {
    <opt_name>  <.opt_ws> \{
        <Perlito5::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', ($<Perlito5::Grammar::Regex.rule>->flat())->perl;
        my $source = $<opt_name> 
            . '{ ' .
                'my $grammar = $_[0]; ' .
                'my $str     = $_[1]; ' .
                'my $pos     = $_[2]; ' .
                'my $MATCH = Perlito5::Match->new( str => $str, from => $pos, to => $pos, bool => 1 ); ' .
                '$MATCH->bool = ( ' .
                    ($<Perlito5::Grammar::Regex.rule>->flat())->emit_perl5() .
                '); ' .
                '$MATCH; ' 
            . '}';
        #say 'Intermediate code: ', $source;
        my $ast = Perlito5::Grammar->sub_def( $source, 0 );
        # say 'Intermediate ast: ', $ast->flat;
        make $ast->flat();
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

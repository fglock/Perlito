use v6;

grammar Perlito6::Grammar {

use Perlito6::Expression;
use Perlito6::Grammar::Regex;
use Perlito6::Grammar::Control;
 
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
        { make ~$<namespace_before_ident> }
    | ''  
        { make '' }
}

token pod_begin {
    |   \n '=end' \N*
    |   . \N* <.pod_begin>
}

token ws {
    [
    |    '#' \N*
    |    \n [
            |  '=begin'  <.pod_begin>
            |  '=for'    <.pod_begin>  # fixme
            |  ''
            ]
    |    \s
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
        make CompUnit.new(
            name        => $$<full_ident>,
            body        => $$<exp_stmts>,
        )
    }
}

token declarator {
     'my' | 'state' | 'has' 
}

token exp_stmts2 { <exp_stmts> { make $$<exp_stmts> } }

token exp {
    <Perlito6::Expression.exp_parse>
        { make $$<Perlito6::Expression.exp_parse> }
}

token exp2 {
    <Perlito6::Expression.exp_parse>
        { make $$<Perlito6::Expression.exp_parse> }
}

token opt_ident {  
    | <ident>  { make $$<ident> }
    | ''       { make 'postcircumfix:<( )>' }
}

token opt_type {
    |   '::'?  <full_ident>   { make $$<full_ident> }
    |   ''                    { make '' }
}

token var_sigil     { \$ |\% |\@ |\& }

token var_twigil    { [ \. | \! | \^ | \* ]? }

token var_name      { <full_ident> | '/' | <digit> }

token var_ident {
    <var_sigil> <var_twigil> <optional_namespace_before_ident> <var_name>
    {
        make Var.new(
            sigil       => ~$<var_sigil>,
            twigil      => ~$<var_twigil>,
            namespace   => $$<optional_namespace_before_ident>,
            name        => ~$<var_name>,
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
    { make Val::Num.new( num => ~$/ ) }
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
        { make "\\" ~ $<single_quoted_unescape> }
    |  \\ \'  <single_quoted_unescape>  
        { make '\'' ~ $<single_quoted_unescape> }
    |  \\   <single_quoted_unescape>  
        { make "\\" ~ $<single_quoted_unescape> }
    |  <char_any_single_quote> <single_quoted_unescape>
        { make $<char_any_single_quote> ~ $<single_quoted_unescape> }
    |  ''    
}

token char_any_double_quote {
    <!before   [ \" | \$ | \@ | \% | \{ ] > .
    [ <!before [ \" | \$ | \@ | \% | \{ | \\ ] > . ]*
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
            { make ~$<char_any> }
        ]
    |  <char_any_double_quote> 
        { make ~$<char_any_double_quote> }
}

token double_quoted_buf {
    | <before \$ > 
        [ <before \$ <.var_twigil> <.ident> > <Perlito6::Expression.operator> 
            { make ($$<Perlito6::Expression.operator>)[1] }
        | <char_any>  
            { make Val::Buf.new( buf => ~$<char_any> ) }
        ]
    | <before \@ > 
        [ <before \@ <.var_twigil> <.ident> > <Perlito6::Expression.operator> '[]'
            { make ($$<Perlito6::Expression.operator>)[1] }
        | <char_any>  
            { make Val::Buf.new( buf => ~$<char_any> ) }
        ]
    | <before \% >
        [ <before \% <.var_twigil> <.ident> > <Perlito6::Expression.operator> '{}'
            { make ($$<Perlito6::Expression.operator>)[1] }
        | <char_any>  
            { make Val::Buf.new( buf => ~$<char_any> ) }
        ]
    | \{ <exp_stmts> \}
            { make Do.new( block => Lit::Block.new( stmts => $$<exp_stmts> ) ) }
    | <double_quoted_unescape> 
        { make Val::Buf.new( buf => $$<double_quoted_unescape> ) }
}

token val_buf {
    | \" <double_quoted_buf>*      \" 
        { 
            my $args = $<double_quoted_buf>;
            if !$args {
                make Val::Buf.new( buf => '' )
            }
            else {
                make Apply.new( 
                    namespace => '',
                    code => 'list:<~>',
                    arguments => ($<double_quoted_buf>)>>.capture, 
                )
            }
        }
    | \' <single_quoted_unescape>  \' 
        { make Val::Buf.new( buf => $$<single_quoted_unescape> ) }
}

token digits {
    \d+
}

token val_int {
    \d+
    { make Val::Int.new( int => ~$/ ) }
}

token exp_stmts {
    <Perlito6::Expression.delimited_statement>*
    { make $<Perlito6::Expression.delimited_statement>>>.capture }
}

token opt_name {  <ident>?  }

token var_invocant {
    |  <var_ident> \:    { make $$<var_ident> }
    |  { make Var.new( 
            sigil  => '$',
            twigil => '',
            name   => 'self',
         ) 
       }
}

token args_sig {
    <var_invocant>
    <.opt_ws> 
    # TODO - Perlito6::Expression.list_parse / exp_mapping == positional / named 
    <Perlito6::Expression.list_parse>
    {
        # say ' invocant: ', ($$<var_invocant>).perl;
        # say ' positional: ', ($$<>).perl;
        make Sig.new( 
            invocant    => $$<var_invocant>, 
            positional  => Perlito6::Expression::expand_list(($$<Perlito6::Expression.list_parse>){'exp'}), 
            named       => { } );
    }
}

token method_sig {
    |   <.opt_ws> \( <.opt_ws>  <args_sig>  <.opt_ws>  \)
        { make $$<args_sig> }
    |   { make Sig.new( 
            invocant => Var.new( 
                sigil  => '$',
                twigil => '',
                name   => 'self' ), 
            positional => [ ], 
            named => { } ) }
}

token method_def {
    <opt_name>  <.opt_ws> 
    <method_sig>
    <.opt_ws> \{ <.opt_ws>  
          <exp_stmts> 
        <.opt_ws> 
    [   \}     | { die 'Syntax Error in method \'.', $$<name>, '\' near pos=', $/.to; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        make Method.new( name => $$<opt_name>, sig => $$<method_sig>, block => $$<exp_stmts> );
    }
}

token sub_def {
    <opt_name>  <.opt_ws> 
    <method_sig>
    <.opt_ws> \{ <.opt_ws>  
          <exp_stmts> <.opt_ws> 
    [   \}     | { die 'Syntax Error in sub \'', $$<name>, '\''; } ]
    { make Sub.new( name => $$<opt_name>, sig => $$<method_sig>, block => $$<exp_stmts> ) }
}

token token {
    <opt_name>  <.opt_ws> \{
        <Perlito6::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', ($$<Perlito6::Grammar::Regex.rule>).perl;
        my $source = $<opt_name> ~ ' ( $grammar: $str, $pos ) { ' ~
            'my $MATCH; $MATCH = Perlito6::Match.new( str => $str, from => $pos, to => $pos, bool => 1 ); ' ~ 
            '$MATCH.bool = ( ' ~
                ($$<Perlito6::Grammar::Regex.rule>).emit_perl6() ~
            '); ' ~
            '$MATCH }';
        #say 'Intermediate code: ', $source;
        my $ast = Perlito6::Grammar.method_def( $source, 0 );
        # say 'Intermediate ast: ', $$ast.perl;
        make $$ast;
    }
}

}

=begin

=head1 NAME 

Perlito6::Grammar - Grammar for Perlito

=head1 SYNOPSIS

    my $match = $source.parse;
    ($$match).perl;    # generated Perlito AST

=head1 DESCRIPTION

This module generates a syntax tree for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2010, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

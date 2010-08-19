use v6;

grammar MiniPerl6::Grammar {

use MiniPerl6::Expression;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Grammar::Control;
 
my $Class_name;  # for diagnostic messages
sub get_class_name { $Class_name } 

token ident {
    [ <!before \d ><.word> | _ ]   [ <.word> | _ | <.digit> ]*
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

token parse {
    | <comp_unit>
        [
        |   <parse>
            { make [ $$<comp_unit>, @( $$<parse> ) ] }
        |   { make [ $$<comp_unit> ] }
        ]
    | { make [] }
}

token comp_unit {
    <.ws>? [ \; <.ws>? ]?
    [ 'use' <.ws> 'v6'  [ '-' <ident> ]?  <.ws>? \; <.ws>? ]?
    
    [ 'class' | 'grammar' ]  <.ws> <full_ident> <.ws>? 
    '{'
        { $Class_name = ~$<full_ident> }
        <.ws>?
        <exp_stmts>
        <.ws>?
    '}'
    <.ws>? [ \; <.ws>? ]?
    {
        make CompUnit.new(
            name        => $$<full_ident>,
            attributes  => { },
            methods     => { },
            body        => $$<exp_stmts>,
        )
    }
}

token hyper_op {
    '>>'?
}

token declarator {
     'my' | 'state' | 'has' 
}

token exp_stmts2 { <exp_stmts> { make $$<exp_stmts> } }

token exp {
    <MiniPerl6::Expression.exp_parse>
        { make $$<MiniPerl6::Expression.exp_parse> }
}

token exp2 {
    <MiniPerl6::Expression.exp_parse>
        { make $$<MiniPerl6::Expression.exp_parse> }
}

token opt_ident {  
    | <ident>  { make $$<ident> }
    | ''     { make 'postcircumfix:<( )>' }
}

token opt_type {
    |   '::'?  <full_ident>   { make $$<full_ident> }
    |   ''                              { make '' }
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

token val {
    | <val_undef>  { make $$<val_undef> }  # undef
    | <val_num>    { make $$<val_num>   }  # 123.456
    | <val_int>    { make $$<val_int>   }  # 123
    | <val_bit>    { make $$<val_bit>   }  # True, False
    | <val_buf>    { make $$<val_buf>   }  # 'moose'
}

token val_bit {
    | 'True'       { make Val::Bit.new( bit => 1 ) }
    | 'False'      { make Val::Bit.new( bit => 0 ) }
}

token digits {  \d+  }

token val_undef {
    undef <!before \w >
    { make Val::Undef.new( ) }
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

token char_any_double_quote {
    <!before \" > .
    [ <!before [ \" | \\ ] > . ]*
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

token double_quoted_unescape {
    |  \\ n  <double_quoted_unescape>  
        { make Main.newline ~ $<double_quoted_unescape> }
    |  \\ <char_any>  <double_quoted_unescape>  
        { make $<char_any> ~ $<double_quoted_unescape> }
    |  <char_any_double_quote> <double_quoted_unescape>
        { make $<char_any_double_quote> ~ $<double_quoted_unescape> }
    |  ''    
}

token val_buf {
    | \" <double_quoted_unescape>  \" { make Val::Buf.new( buf => $$<double_quoted_unescape> ) }
    | \' <single_quoted_unescape>  \' { make Val::Buf.new( buf => $$<single_quoted_unescape> ) }
}

token val_int {
    \d+
    { make Val::Int.new( int => ~$/ ) }
}

token exp_stmts {
    | <MiniPerl6::Expression.statement_parse>
        [
        |   <.opt_ws> [ \; | '' ] <.opt_ws> <exp_stmts>
            <.opt_ws> [ \; <.opt_ws> | '' ]
            { make [ $$<MiniPerl6::Expression.statement_parse>, @( $$<exp_stmts> ) ] }
        |   <.opt_ws> [ \; <.opt_ws> | '' ]
            { make [ $$<MiniPerl6::Expression.statement_parse> ] }
        ]
    | { make [] }
}

token lit {
    <lit_object> { make $$<lit_object> }  # Tree.new(a => x, b => y);
}

token lit_object {
    '::'
    <full_ident>
    \( 
    [
        <.opt_ws> <exp_mapping> <.opt_ws> \)
        {
            # say 'Parsing Lit::Object ', $$<full_ident>, ($$<exp_mapping>).perl;
            make Lit::Object.new(
                class  => $$<full_ident>,
                fields => $$<exp_mapping>
            )
        }
    | { say '*** Syntax Error parsing Constructor'; die() }
    ]
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
    # TODO - MiniPerl6::Expression.list_parse / exp_mapping == positional / named 
    <MiniPerl6::Expression.list_parse>
    {
        # say ' invocant: ', ($$<var_invocant>).perl;
        # say ' positional: ', ($$<>).perl;
        make Sig.new( 
            invocant    => $$<var_invocant>, 
            positional  => MiniPerl6::Expression::expand_list(($$<MiniPerl6::Expression.list_parse>){'exp'}), 
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
    method
    <.ws>  <opt_name>  <.opt_ws> 
    <method_sig>
    <.opt_ws> \{ <.opt_ws>  
          # { say ' parsing statement list ' }
          <exp_stmts> 
          # { say ' got statement list ', ($$<exp_stmts>).perl } 
        <.opt_ws> 
    [   \}     | { say '*** Syntax Error in method \'', get_class_name(), '.', $$<name>, '\' near pos=', $/.to; die 'error in Block'; } ]
    {
        # say ' block: ', ($$<exp_stmts>).perl;
        make Method.new( name => $$<opt_name>, sig => $$<method_sig>, block => $$<exp_stmts> );
    }
}

token sub_def {
    sub
    <.ws>  <opt_name>  <.opt_ws> 
    <method_sig>
    <.opt_ws> \{ <.opt_ws>  
          <exp_stmts> <.opt_ws> 
    [   \}     | { say '*** Syntax Error in sub \'', $$<name>, '\''; die 'error in Block'; } ]
    { make Sub.new( name => $$<opt_name>, sig => $$<method_sig>, block => $$<exp_stmts> ) }
}

token token {
    # { say 'parsing Token' }
    token
    <.ws>  <opt_name>  <.opt_ws> \{
        <MiniPerl6::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', ($$<MiniPerl6::Grammar::Regex.rule>).perl;
        my $source = 'method ' ~ $<opt_name> ~ ' ( $grammar: $str, $pos ) { ' ~
            'my $MATCH; $MATCH = MiniPerl6::Match.new( \'str\' => $str, \'from\' => $pos, \'to\' => $pos, \'bool\' => 1 ); ' ~ 
            '$MATCH.bool = ( ' ~
                ($$<MiniPerl6::Grammar::Regex.rule>).emit ~
            '); ' ~
            '$MATCH }';
        #say 'Intermediate code: ', $source;
        my $ast = MiniPerl6::Grammar.method_def( $source, 0 );
        # say 'Intermediate ast: ', $$ast.emit;
        make $$ast;
    }
}

}

=begin

=head1 NAME 

MiniPerl6::Grammar - Grammar for MiniPerl6

=head1 SYNOPSIS

    my $match = $source.parse;
    ($$match).perl;    # generated MiniPerl6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2010 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

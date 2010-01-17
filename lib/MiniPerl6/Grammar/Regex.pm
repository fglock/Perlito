
use v6;

grammar MiniPerl6::Grammar::Regex {

my %rule_terms;

token ws {  <.MiniPerl6::Grammar.ws>  }

token rule_ident {  <.MiniPerl6::Grammar.full_ident> | <digit> }

token any { . }

token literal {
    |  \\ .        <literal>
    |  <!before \' > .  <literal>
    |  ''
}

token metasyntax_exp {
    [ 
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.string_code> \}
    |  \<  <.metasyntax_exp>  \>
    |  <!before \> > . 
    ]
    [ <metasyntax_exp> | '' ]
}

token char_range {
    [ 
    |  \\ .
    |  <!before \] > . 
    ]
    [ <char_range> | '' ]
}

token char_class {
    |  <.rule_ident>
    |  \[  <.char_range>  \]
}

# XXX - not needed
token string_code {
    # bootstrap 'code'
    [ 
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.string_code> \}
    |  <!before \} > . 
    ]
    [ <string_code> | '' ]
}

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    # XXX - call MiniPerl6 'Statement List'
    <.string_code>
    { make ~$/ }
}

token named_capture_body {
    | \(  <rule>        \)  { make { 'capturing_group' => $$<rule> ,} } 
    | \[  <rule>        \]  { make $$<rule> } 
    | \<  <metasyntax_exp>  \>  
            { make Rul::Subrule.new( 'metasyntax' => $$<metasyntax_exp> ) }
    | { die 'invalid alias syntax' }
}

token variables {
    |
        '$<'
        <rule_ident> \> 
        { make '$/{' ~ '\'' ~ $<rule_ident> ~ '\'' ~ '}' }
    |
        # TODO
        <MiniPerl6::Grammar.var_sigil> 
        <MiniPerl6::Grammar.digits>
        { make $<MiniPerl6::Grammar.var_sigil> ~ '/[' ~ $<MiniPerl6::Grammar.digits> ~ ']' }
    |
        <MiniPerl6::Grammar.var_sigil> 
        <MiniPerl6::Grammar.var_twigil> 
        <MiniPerl6::Grammar.full_ident> 
        {
            make Rul::Var.new( 
                    'sigil'  => ~$<MiniPerl6::Grammar.var_sigil>,
                    'twigil' => ~$<MiniPerl6::Grammar.var_twigil>,
                    'name'   => ~$<MiniPerl6::Grammar.full_ident>
                   )
        }
}

token rule_terms {
    |   '('
        <rule> \)
        { make Rul::Capture.new( 'rule_exp' => $$<rule> ) }
    |   '<('
        <rule>  ')>'
        { make Rul::CaptureResult.new( 'rule_exp' => $$<rule> ) }
    |   '<after'
        <.ws> <rule> \> 
        { make Rul::After.new( 'rule_exp' => $$<rule> ) }
    |   '<before'
        <.ws> <rule> \> 
        { make Rul::Before.new( 'rule_exp' => $$<rule> ) }
    |   '<!before'
        <.ws> <rule> \> 
        { make Rul::NotBefore.new( 'rule_exp' => $$<rule> ) }
    |   '<!'
        # TODO
        <metasyntax_exp> \> 
        { make { negate  => { 'metasyntax' => $$<metasyntax_exp> } } }
    |   '<+'
        # TODO
        <char_class>  \> 
        { make Rul::CharClass.new( 'chars' => ~$<char_class> ) }
    |   '<-'
        # TODO
        <char_class> \>
        { make Rul::NegateCharClass.new( 'chars' => ~$<char_class> ) }
    |   \'
        <literal> \'
        { make Rul::Constant.new( 'constant' => $$<literal> ) }
    |   # XXX - obsolete syntax
        \< \'
        <literal> \' \>
        { make Rul::Constant.new( 'constant' => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say 'matching < variables ...' }
            {
                # say 'found < hash-variable >';
                make Rul::InterpolateVar.new( 'var' => $$<variables> )
            }
        |
            \?
            # TODO 
            <metasyntax_exp>  \>
            { make Rul::SubruleNoCapture.new( 'metasyntax' => $$<metasyntax_exp> ) }
        |
            \.
            <metasyntax_exp>  \>
            { make Rul::SubruleNoCapture.new( 'metasyntax' => $$<metasyntax_exp> ) }
        |
            # TODO
            <metasyntax_exp>  \>
            { make Rul::Subrule.new( 'metasyntax' => $$<metasyntax_exp> ) }
        ]
    |   \{ 
        <parsed_code>  \}
        { make Rul::Block.new( 'closure' => $$<parsed_code> ) }
    |   \\  
        [
# TODO
#        | [ x | X ] <[ 0..9 a..f A..F ]]>+
#          #  \x0021    \X0021
#          { make Rul::SpecialChar.new( char => '\\' ~ $/ ) }
#        | [ o | O ] <[ 0..7 ]>+
#          #  \x0021    \X0021
#          { make Rul::SpecialChar.new( char => '\\' ~ $/ ) }
#        | ( x | X | o | O ) \[ (<-[ \] ]>*) \]
#          #  \x[0021]  \X[0021]
#          { make Rul::SpecialChar.new( char => '\\' ~ $0 ~ $1 ) }
        | <any>
          #  \e  \E
          { make Rul::SpecialChar.new( 'char' => $$<any> ) }
        ]
    |   \. 
        { make Rul::Dot.new() }
    |   '[' 
        <rule> ']' 
        { make $$<rule> }

}

=for later
    |   ':::' { make { 'colon' => ':::' ,} }
    |   ':?'  { make { 'colon' => ':?' ,} }
    |   ':+'  { make { 'colon' => ':+' ,} }
    |   '::'  { make { 'colon' => '::' ,} }
    |   ':'   { make { 'colon' => ':'  ,} }
    |   '$$'  { make { 'colon' => '$$' ,} }
    |   '$'   { make { 'colon' => '$'  ,} }


# TODO - parser error ???
#    |   '^^' { make { 'colon' => '^^' ,} }
#    |   '^'  { make { 'colon' => '^'  ,} } }
#    |   '»'  { make { 'colon' => '>>' ,} } }
#    |   '«'  { make { 'colon' => '<<' ,} } }

    |   '<<'  { make { 'colon' => '<<' ,} }     
    |   '>>'  { make { 'colon' => '>>' ,} }     
    |   ':i' 
        <.ws> <rule> 
        { make { 'modifier' => 'ignorecase', 'rule_exp' => $$<rule>, } }     
    |   ':ignorecase' 
        <.ws> <rule> 
        { make { 'modifier' => 'ignorecase', 'rule_exp' => $$<rule>, } }     
    |   ':s' 
        <.ws> <rule> 
        { make { 'modifier' => 'sigspace',   'rule_exp' => $$<rule>, } }     
    |   ':sigspace' 
        <.ws> <rule> 
        { make { 'modifier' => 'sigspace',   'rule_exp' => $$<rule>, } }     
    |   ':P5' 
        <.ws> <rule> 
        { make { 'modifier' => 'Perl5',  'rule_exp' => $$<rule>, } }     
    |   ':Perl5' 
        <.ws> <rule> 
        { make { 'modifier' => 'Perl5',  'rule_exp' => $$<rule>, } }     
    |   ':bytes' 
        <.ws> <rule> 
        { make { 'modifier' => 'bytes',  'rule_exp' => $$<rule>, } }     
    |   ':codes' 
        <.ws> <rule> 
        { make { 'modifier' => 'codes',  'rule_exp' => $$<rule>, } }     
    |   ':graphs' 
        <.ws> <rule> 
        { make { 'modifier' => 'graphs', 'rule_exp' => $$<rule>, } }     
    |   ':langs' 
        <.ws> <rule> 
        { make { 'modifier' => 'langs',  'rule_exp' => $$<rule>, } } }
}
=cut

token rule_term {
    |  
       # { say 'matching variables' } 
       <variables>
       [  <.ws>? <':='> <.ws>? <named_capture_body>
          { 
            make Rul::NamedCapture.new(
                'rule_exp' =>  $$<named_capture_body>,
                'capture_ident' => $$<variables>
            ); 
          }
       |
          { 
            make $$<variables>
          }
       ]
    | 
        # { say 'matching terms'; }
        <rule_terms>
        { 
            #print 'term: ', Dumper( $_[0]->data );
            make $$<rule_terms> 
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { make Rul::Constant.new( 'constant' => $$<any> ) }
}

token quant_exp {
    |   <'**'> <.MiniPerl6::Grammar.opt_ws> \{  <parsed_code>  \}
        { make { 'closure' => $$<parsed_code> } }
    |   [  \? | \* | \+  ]
}

token greedy_exp {   \?  |  \+  |  ''  }

token quantifier {
    #|   <.MiniPerl6::Grammar.opt_ws>
    #    <before   \}  |  \]   |  \)   >
    #    XXX   # fail
    #|
        <.MiniPerl6::Grammar.opt_ws>
        <rule_term> 
        <.MiniPerl6::Grammar.opt_ws2>
        [
            <quant_exp> <greedy_exp>
            <.MiniPerl6::Grammar.opt_ws3>
            { make Rul::Quantifier.new(
                    'term'    => $$<rule_term>,
                    'quant'   => $$<quant_exp>,
                    'greedy'  => $$<greedy_exp>,
                    'ws1'     => $$<MiniPerl6::Grammar.opt_ws>,
                    'ws2'     => $$<MiniPerl6::Grammar.opt_ws2>,
                    'ws3'     => $$<MiniPerl6::Grammar.opt_ws3>,
                )
            }
        |
            { make $$<rule_term> }
        ]
}

token concat_list {
    <quantifier>
    [
        <concat_list> 
        { make [ $$<quantifier>, @($$<concat_list>) ] }
    |
        { make [ $$<quantifier> ] }
    ]
    |
        { make [] }
}

token concat_exp {
    <concat_list>
    { make Rul::Concat.new( 'concat' => $$<concat_list> ) }
}

token or_list_exp {
    <concat_exp>
    [
        <'|'>
        <or_list_exp> 
        { make [ $$<concat_exp>, @($$<or_list_exp>) ] }
    |
        { make [ $$<concat_exp> ] }
    ]
    |
        { make [] }
}

token rule {
    [ <.ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list_exp>
    { 
        # say 'found Rule';
        make Rul::Or.new( 'or_list' => $$<or_list_exp> ) 
    }
}

}

=begin

=head1 NAME 

MiniPerl6::Grammar::Regex - Grammar for MiniPerl6 Regex

=head1 SYNOPSIS

    my $match := $source.rule;
    ($$match).perl;    # generated Regex AST

=head1 DESCRIPTION

This module generates a syntax tree for the Regex compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

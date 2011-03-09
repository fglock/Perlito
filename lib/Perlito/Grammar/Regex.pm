
use v6;

grammar Perlito::Grammar::Regex {

my %rule_terms;

token ws {  <.Perlito::Grammar.ws>  }

token rule_ident {  <.Perlito::Grammar.full_ident> | <digit> }

token any { . }

token literal {
    [
    |  \\ .        
    |  <!before \' > .  
    ]*
}

token metasyntax_exp {
    [ 
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.string_code> \}
    |  \<  <.metasyntax_exp>  \>
    |  <!before \> > . 
    ]+
}

token char_range {
    [ 
    |  \\ .
    |  <!before \] > . 
    ]+
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
    ]+
}

token parsed_code {
    # this subrule is overridden inside the perl6 compiler
    # XXX - call Perlito 'Statement List'
    <.string_code>
    { make ~$/ }
}

token named_capture_body {
    | \(  <rule>        \)  { make { capturing_group => $$<rule> ,} } 
    | \[  <rule>        \]  { make $$<rule> } 
    | \<  <metasyntax_exp>  \>  
            { make Rul::Subrule.new( metasyntax => $$<metasyntax_exp>, captures => 1 ) }
    | { die 'invalid alias syntax' }
}

token variables {
    |
        '$<'
        <rule_ident> \> 
        { make '$/{' ~ '\'' ~ $<rule_ident> ~ '\'' ~ '}' }
    |
        # TODO
        <Perlito::Grammar.var_sigil> 
        <Perlito::Grammar.val_int>
        { make $<Perlito::Grammar.var_sigil> ~ '/[' ~ $<Perlito::Grammar.val_int> ~ ']' }
    |
        <Perlito::Grammar.var_sigil> 
        <Perlito::Grammar.var_twigil> 
        <Perlito::Grammar.full_ident> 
        {
            make Rul::Var.new( 
                    sigil  => ~$<Perlito::Grammar.var_sigil>,
                    twigil => ~$<Perlito::Grammar.var_twigil>,
                    name   => ~$<Perlito::Grammar.full_ident>
                   )
        }
}

token rule_terms {
    |   '('
        <rule> \)
        { make Rul::Capture.new( rule_exp => $$<rule> ) }
    |   '<('
        <rule>  ')>'
        { make Rul::CaptureResult.new( rule_exp => $$<rule> ) }
    |   '<after'
        <.ws> <rule> \> 
        { make Rul::After.new( rule_exp => $$<rule> ) }
    |   '<before'
        <.ws> <rule> \> 
        { make Rul::Before.new( rule_exp => $$<rule> ) }
    |   '<!before'
        <.ws> <rule> \> 
        { make Rul::NotBefore.new( rule_exp => $$<rule> ) }
    |   '<!'
        # TODO
        <metasyntax_exp> \> 
        { make { negate  => { metasyntax => $$<metasyntax_exp> } } }
    |   '<+'
        # TODO
        <char_class>  \> 
        { make Rul::CharClass.new( chars => ~$<char_class> ) }
    |   '<-'
        # TODO
        <char_class> \>
        { make Rul::NegateCharClass.new( chars => ~$<char_class> ) }
    |   \'
        <literal> \'
        { make Rul::Constant.new( constant => $$<literal> ) }
    |   # XXX - obsolete syntax
        \< \'
        <literal> \' \>
        { make Rul::Constant.new( constant => $$<literal> ) }
    |   \< 
        [  
            <variables>   \>
            # { say 'matching < variables ...' }
            {
                # say 'found < hash-variable >';
                make Rul::InterpolateVar.new( var => $$<variables> )
            }
        |
            \?
            # TODO 
            <metasyntax_exp>  \>
            { make Rul::Subrule.new( metasyntax => $$<metasyntax_exp>, captures => 0 ) }
        |
            \.
            <metasyntax_exp>  \>
            { make Rul::Subrule.new( metasyntax => $$<metasyntax_exp>, captures => 0 ) }
        |
            # TODO
            <metasyntax_exp>  \>
            { make Rul::Subrule.new( metasyntax => $$<metasyntax_exp>, captures => 1 ) }
        ]
    |   \{ 
        <parsed_code>  \}
        { make Rul::Block.new( closure => $$<parsed_code> ) }
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
          { make Rul::SpecialChar.new( char => $$<any> ) }
        ]
    |   \. 
        { make Rul::Dot.new() }
    |   '[' 
        <rule> ']' 
        { make $$<rule> }

}

=begin later
    |   ':::' { make { colon => ':::' ,} }
    |   ':?'  { make { colon => ':?' ,} }
    |   ':+'  { make { colon => ':+' ,} }
    |   '::'  { make { colon => '::' ,} }
    |   ':'   { make { colon => ':'  ,} }
    |   '$$'  { make { colon => '$$' ,} }
    |   '$'   { make { colon => '$'  ,} }


# TODO - parser error ???
#    |   '^^' { make { colon => '^^' ,} }
#    |   '^'  { make { colon => '^'  ,} } }
#    |   '»'  { make { colon => '>>' ,} } }
#    |   '«'  { make { colon => '<<' ,} } }

    |   '<<'  { make { colon => '<<' ,} }     
    |   '>>'  { make { colon => '>>' ,} }     
    |   ':i' 
        <.ws> <rule> 
        { make { modifier => 'ignorecase', rule_exp => $$<rule>, } }     
    |   ':ignorecase' 
        <.ws> <rule> 
        { make { modifier => 'ignorecase', rule_exp => $$<rule>, } }     
    |   ':s' 
        <.ws> <rule> 
        { make { modifier => 'sigspace',   rule_exp => $$<rule>, } }     
    |   ':sigspace' 
        <.ws> <rule> 
        { make { modifier => 'sigspace',   rule_exp => $$<rule>, } }     
    |   ':P5' 
        <.ws> <rule> 
        { make { modifier => 'Perl5',  rule_exp => $$<rule>, } }     
    |   ':Perl5' 
        <.ws> <rule> 
        { make { modifier => 'Perl5',  rule_exp => $$<rule>, } }     
    |   ':bytes' 
        <.ws> <rule> 
        { make { modifier => 'bytes',  rule_exp => $$<rule>, } }     
    |   ':codes' 
        <.ws> <rule> 
        { make { modifier => 'codes',  rule_exp => $$<rule>, } }     
    |   ':graphs' 
        <.ws> <rule> 
        { make { modifier => 'graphs', rule_exp => $$<rule>, } }     
    |   ':langs' 
        <.ws> <rule> 
        { make { modifier => 'langs',  rule_exp => $$<rule>, } } }
}
=end

token rule_term {
    |  
       # { say 'matching variables' } 
       <variables>
       [  <.ws>? '=' <.ws>? <named_capture_body>
          { 
            make Rul::NamedCapture.new(
                rule_exp =>  $$<named_capture_body>,
                capture_ident => $$<variables>
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
            make $$<rule_terms> 
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { make Rul::Constant.new( constant => $$<any> ) }
}

token quant_exp {
    |   '**'  <.Perlito::Grammar.opt_ws> 
        [
        |  <Perlito::Grammar.val_int>
           { make $$<Perlito::Grammar.val_int> }
        |  <rule_term>
           { make $$<rule_term> }
        ]
    |   [  \? | \* | \+  ]
}

token greedy_exp {   \?  |  \+  |  ''  }

token quantifier {
    <.Perlito::Grammar.opt_ws>
    <rule_term> 
    <.Perlito::Grammar.opt_ws2>
    [
        <quant_exp> <greedy_exp>
        <.Perlito::Grammar.opt_ws3>
        { make Rul::Quantifier.new(
                term    => $$<rule_term>,
                quant   => $$<quant_exp>,
                greedy  => $$<greedy_exp>,
                ws1     => $$<Perlito::Grammar.opt_ws>,
                ws2     => $$<Perlito::Grammar.opt_ws2>,
                ws3     => $$<Perlito::Grammar.opt_ws3>,
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
    { make Rul::Concat.new( concat => $$<concat_list> ) }
}

token or_list_exp {
    <concat_exp>
    [
        '|'
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
        make Rul::Or.new( or_list => $$<or_list_exp> ) 
    }
}

}

=begin

=head1 NAME 

Perlito::Grammar::Regex - Grammar for Perlito Regex

=head1 SYNOPSIS

    my $match = $source.rule;
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

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

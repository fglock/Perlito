package Perlito5::Grammar::Regex;

my %rule_terms;

token ws {  <.Perlito5::Grammar.ws>  }

token rule_ident {  <.Perlito5::Grammar.full_ident> | <digit> }

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
    { $MATCH->capture = $MATCH->flat() }
}

token named_capture_body {
    | \(  <rule>        \)  { $MATCH->capture = { capturing_group => $MATCH->{"rule"}->flat() ,} }
    | \[  <rule>        \]  { $MATCH->capture = $MATCH->{"rule"}->flat() }
    | \<  <metasyntax_exp>  \>
            { $MATCH->capture = Rul::Subrule->new( metasyntax => $MATCH->{"metasyntax_exp"}->flat(), captures => 1 ) }
    | { die 'invalid alias syntax' }
}

token rule_terms {
    |   '('
        <rule> \)
        { $MATCH->capture = Rul::Capture->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<('
        <rule>  ')>'
        { $MATCH->capture = Rul::CaptureResult->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<after'
        <.ws> <rule> \>
        { $MATCH->capture = Rul::After->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<before'
        <.ws> <rule> \>
        { $MATCH->capture = Rul::Before->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<!before'
        <.ws> <rule> \>
        { $MATCH->capture = Rul::NotBefore->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<!'
        # TODO
        <metasyntax_exp> \>
        { $MATCH->capture = { negate  => { metasyntax => $MATCH->{"metasyntax_exp"}->flat() } } }
    |   '<+'
        # TODO
        <char_class>  \>
        { $MATCH->capture = Rul::CharClass->new( chars => '' . $MATCH->{"char_class"} ) }
    |   '<-'
        # TODO
        <char_class> \>
        { $MATCH->capture = Rul::NegateCharClass->new( chars => '' . $MATCH->{"char_class"} ) }
    |   \'
        <literal> \'
        { $MATCH->capture = Rul::Constant->new( constant => $MATCH->{"literal"}->flat() ) }
    |   # XXX - obsolete syntax
        \< \'
        <literal> \' \>
        { $MATCH->capture = Rul::Constant->new( constant => $MATCH->{"literal"}->flat() ) }
    |   \<
        [
            \.
            <metasyntax_exp>  \>
            { $MATCH->capture = Rul::Subrule->new( metasyntax => $MATCH->{"metasyntax_exp"}->flat(), captures => 0 ) }
        |
            <metasyntax_exp>  \>
            { $MATCH->capture = Rul::Subrule->new( metasyntax => $MATCH->{"metasyntax_exp"}->flat(), captures => 1 ) }
        ]
    |   \{
        <parsed_code>  \}
        { $MATCH->capture = Rul::Block->new( closure => $MATCH->{"parsed_code"}->flat() ) }
    |   \\
        [
        | c \[ <Perlito5::Grammar.digits> \]
          { $MATCH->capture = Rul::Constant->new( constant => chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) ) }
        | c <Perlito5::Grammar.digits>
          { $MATCH->capture = Rul::Constant->new( constant => chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) ) }
        | <any>
          #  \e  \E
          { $MATCH->capture = Rul::SpecialChar->new( char => $MATCH->{"any"}->flat() ) }
        ]
    |   \.
        { $MATCH->capture = Rul::Dot->new() }
    |   '['
        <rule> ']'
        { $MATCH->capture = $MATCH->{"rule"}->flat() }

}

token rule_term {
    |
        # { say 'matching terms'; }
        <rule_terms>
        {
            $MATCH->capture = $MATCH->{"rule_terms"}->flat()
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { $MATCH->capture = Rul::Constant->new( constant => $MATCH->{"any"}->flat() ) }
}

token quant_exp {
    |   '**'  <.Perlito5::Grammar.opt_ws>
        [
        |  <Perlito5::Grammar.val_int>
           { $MATCH->capture = $MATCH->{"Perlito5::Grammar.val_int"}->flat() }
        |  <rule_term>
           { $MATCH->capture = $MATCH->{"rule_term"}->flat() }
        ]
    |   [  \? | \* | \+  ]
}

token greedy_exp {   \?  |  \+  |  ''  }

token quantifier {
    <Perlito5::Grammar.opt_ws>
    <rule_term>
    <Perlito5::Grammar.opt_ws2>
    [
        <quant_exp> <greedy_exp>
        <Perlito5::Grammar.opt_ws3>
        { $MATCH->capture = Rul::Quantifier->new(
                term    => $MATCH->{"rule_term"}->flat(),
                quant   => $MATCH->{"quant_exp"}->flat(),
                greedy  => $MATCH->{"greedy_exp"}->flat(),
                ws1     => $MATCH->{"Perlito5::Grammar.opt_ws"}->flat(),
                ws2     => $MATCH->{"Perlito5::Grammar.opt_ws2"}->flat(),
                ws3     => $MATCH->{"Perlito5::Grammar.opt_ws3"}->flat(),
            )
        }
    |
        { $MATCH->capture = $MATCH->{"rule_term"}->flat() }
    ]
}

token concat_list {
    <quantifier>
    [
        <concat_list>
        { $MATCH->capture = [ $MATCH->{"quantifier"}->flat(), @{$MATCH->{"concat_list"}->flat()} ] }
    |
        { $MATCH->capture = [ $MATCH->{"quantifier"}->flat() ] }
    ]
    |
        { $MATCH->capture = [] }
}

token concat_exp {
    <concat_list>
    { $MATCH->capture = Rul::Concat->new( concat => $MATCH->{"concat_list"}->flat() ) }
}

token or_list_exp {
    <concat_exp>
    [
        '|'
        <or_list_exp>
        { $MATCH->capture = [ $MATCH->{"concat_exp"}->flat(), @{$MATCH->{"or_list_exp"}->flat()} ] }
    |
        { $MATCH->capture = [ $MATCH->{"concat_exp"}->flat() ] }
    ]
    |
        { $MATCH->capture = [] }
}

token rule {
    [ <.ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list_exp>
    {
        # say 'found Rule';
        $MATCH->capture = Rul::Or->new( or_list => $MATCH->{"or_list_exp"}->flat() )
    }
}

=begin

=head1 NAME

Perlito5::Grammar::Regex - Grammar for Perlito Regex

=head1 SYNOPSIS

    my $match = $source.rule;
    $match->flat();    # generated Regex AST

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

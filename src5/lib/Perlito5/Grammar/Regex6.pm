package Perlito5::Grammar::Regex6;

use Perlito5::Grammar::Precedence;

# this module implements a Recursive descent parser using (more or less) the Perl 6 "token" syntax
#
# http://en.wikipedia.org/wiki/Recursive_descent_parser

# Here we register the "token" keyword as a language term.
#
# "token" is not a Perl5 word, but Perl6 - but it is useful inside the grammar compiler

token term_token {
    'token' <.Perlito5::Grammar::Space::ws> <Perlito5::Grammar::ident>  <.Perlito5::Grammar::Space::opt_ws> \{
        <Perlito5::Grammar::Regex6::rule>
    \}
    {
        my $source = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::ident"})
            . '{ ' .
                'my $str     = $_[0]; ' .
                'my $pos     = $_[1]; ' .
                'my $MATCH = { str => $str, from => $pos, to => $pos }; ' .
                'my @STACK; ' .
                'my $tmp = ( ' .
                    Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Regex6::rule"})->emit_perl5() .
                '); ' .
                '$tmp ? $MATCH : undef; '
            . '}';
        $source = [ split //, $source ];
        my $ast = Perlito5::Grammar::Block::named_sub_def( $source, 0 );
        $MATCH->{capture} = [ 'term', Perlito5::Match::flat($ast) ];
    }
};

Perlito5::Grammar::Precedence::add_term( 'token', \&term_token );

# this is the "grammar grammar"

token any { . };

token literal {
    [
    |  \\ .
    |  <!before \' > .
    ]*
};

token metasyntax_exp {
    [   <!before '>' > .   ]+
};

token string_code {
    [
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.string_code> \}
    |  <!before \} > .
    ]+
};

token parsed_code {
    <.string_code>
    { $MATCH->{capture} = Perlito5::Match::flat($MATCH) }
};

token rule_term {
    |   '<before'
        <.Perlito5::Grammar::Space::ws> <rule> \>
        { $MATCH->{capture} = Perlito5::Rul::Before->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
    |   '<!before'
        <.Perlito5::Grammar::Space::ws> <rule> \>
        { $MATCH->{capture} = Perlito5::Rul::NotBefore->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
    |   \'
        <literal> \'
        { $MATCH->{capture} = Perlito5::Rul::Constant->new( constant => Perlito5::Match::flat($MATCH->{literal}) ) }
    |   \<
        [
            \.
            <metasyntax_exp>  \>
            { $MATCH->{capture} = Perlito5::Rul::Subrule->new( metasyntax => Perlito5::Match::flat($MATCH->{metasyntax_exp}), captures => 0 ) }
        |
            <metasyntax_exp>  \>
            { $MATCH->{capture} = Perlito5::Rul::Subrule->new( metasyntax => Perlito5::Match::flat($MATCH->{metasyntax_exp}), captures => 1 ) }
        ]
    |   \{
        <parsed_code>  \}
        { $MATCH->{capture} = Perlito5::Rul::Block->new( closure => Perlito5::Match::flat($MATCH->{parsed_code}) ) }
    |   \\
        [
        | 'c' <Perlito5::Grammar::Number::digits>
          { $MATCH->{capture} = Perlito5::Rul::Constant->new( constant => chr( Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::digits"}) ) ) }
        | <any>
          #  \e  \E
          { $MATCH->{capture} = Perlito5::Rul::SpecialChar->new( char => Perlito5::Match::flat($MATCH->{any}) ) }
        ]
    |   \.
        { $MATCH->{capture} = Perlito5::Rul::Dot->new() }
    |   '['
        <rule> ']'
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule}) }

};

token quant_exp  {   \? | \* | \+  };

token quantifier {
    <Perlito5::Grammar::Space::opt_ws>
    <rule_term>
    <Perlito5::Grammar::Space::opt_ws>
    [
        <quant_exp>
        <Perlito5::Grammar::Space::opt_ws>
        { $MATCH->{capture} = Perlito5::Rul::Quantifier->new(
                term    => Perlito5::Match::flat($MATCH->{rule_term}),
                quant   => Perlito5::Match::flat($MATCH->{quant_exp}),
            )
        }
    |
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule_term}) }
    ]
};

token concat_list {
    <quantifier>
    [
        <concat_list>
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{quantifier}), @{Perlito5::Match::flat($MATCH->{concat_list})} ] }
    |
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{quantifier}) ] }
    ]
    |
        { $MATCH->{capture} = [] }
};

token concat_exp {
    <concat_list>
    { $MATCH->{capture} = Perlito5::Rul::Concat->new( concat => Perlito5::Match::flat($MATCH->{concat_list}) ) }
};

token or_list_exp {
    <concat_exp>
    [
        '|'
        <or_list_exp>
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{concat_exp}), @{Perlito5::Match::flat($MATCH->{or_list_exp})} ] }
    |
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{concat_exp}) ] }
    ]
    |
        { $MATCH->{capture} = [] }
};

token rule {
    [ <.Perlito5::Grammar::Space::ws>? '|' | '' ]
    # { say 'trying M::G::Perlito5::Rule on ', $s }
    <or_list_exp>
    {
        # say 'found Perlito5::Rule';
        $MATCH->{capture} = Perlito5::Rul::Or->new( or_list => Perlito5::Match::flat($MATCH->{or_list_exp}) )
    }
};

=begin

=head1 NAME

Perlito5::Grammar::Regex6 - Grammar for Perlito Grammar

=head1 SYNOPSIS

    my $match = Perlito5::Grammar::Regex6::rule( $source, $pos );
    Perlito5::Match::flat($match);    # generated Regex6 AST

=head1 DESCRIPTION

This module generates a syntax tree for the Regex6 compiler.

This is not useful during runtime, and this module should never be loaded in the compiled programs.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

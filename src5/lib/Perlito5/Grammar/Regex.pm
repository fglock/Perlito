package Perlito5::Grammar::Regex;

use Perlito5::Precedence;

    # this module implements a Recursive descent parser
    # using (more or less) the Perl 6 "token" algorithm
    #
    # the parsing process is based on Perl 6:
    # "Perl 6 "sandwiches" an operator-precedence parser in between two Recursive descent parsers"
    #
    # see the precedence_parse() implementation for the operator-precedence parser (Perlito5::Precedence)
    #
    # http://en.wikipedia.org/wiki/Recursive_descent_parser
    #


# Here we register the "token" keyword as a language term.
#
# "token" is not a Perl5 word, but Perl6 - but it is useful inside the grammar compiler

token token {
    <Perlito5::Grammar.ident>  <.Perlito5::Grammar::Space.opt_ws> \{
        <Perlito5::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', Perlito5::Match::flat(($MATCH->{"Perlito5::Grammar::Regex.rule"}))->perl;
        my $source = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.ident"})
            . '{ ' .
                'my $grammar = $_[0]; ' .
                'my $str     = $_[1]; ' .
                'my $pos     = $_[2]; ' .
                'my $MATCH = { str => $str, from => $pos, to => $pos }; ' .
                'my $tmp = ( ' .
                    Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Regex.rule"})->emit_perl5() .
                '); ' .
                '$tmp ? $MATCH : 0; '
            . '}';
        #say 'Intermediate code: ', $source;
        my $ast = Perlito5::Grammar->named_sub_def( $source, 0 );
        # say 'Intermediate ast: ', $ast->flat;
        $MATCH->{capture} = Perlito5::Match::flat($ast);
    }
};

token term_token {
    'token' <.Perlito5::Grammar::Space.ws> <token>
                { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{token})       ] }
};

Perlito5::Precedence::add_term( 'token', sub { Perlito5::Grammar::Regex->term_token($_[0], $_[1]) } );


# this is the "grammar grammar"


token any { . };

token literal {
    [
    |  \\ .
    |  <!before \' > .
    ]*
};

token metasyntax_exp {
    [
    |  \\ .
    |  \'  <.literal>     \'
    |  \{  <.string_code> \}
    |  \<  <.metasyntax_exp>  \>
    |  <!before \> > .
    ]+
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

token rule_terms {
    |   '<before'
        <.Perlito5::Grammar::Space.ws> <rule> \>
        { $MATCH->{capture} = Rul::Before->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
    |   '<!before'
        <.Perlito5::Grammar::Space.ws> <rule> \>
        { $MATCH->{capture} = Rul::NotBefore->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
    |   \'
        <literal> \'
        { $MATCH->{capture} = Rul::Constant->new( constant => Perlito5::Match::flat($MATCH->{literal}) ) }
    |   \<
        [
            \.
            <metasyntax_exp>  \>
            { $MATCH->{capture} = Rul::Perlito5::AST::Subrule->new( metasyntax => Perlito5::Match::flat($MATCH->{metasyntax_exp}), captures => 0 ) }
        |
            <metasyntax_exp>  \>
            { $MATCH->{capture} = Rul::Perlito5::AST::Subrule->new( metasyntax => Perlito5::Match::flat($MATCH->{metasyntax_exp}), captures => 1 ) }
        ]
    |   \{
        <parsed_code>  \}
        { $MATCH->{capture} = Rul::Block->new( closure => Perlito5::Match::flat($MATCH->{parsed_code}) ) }
    |   \\
        [
        | c \[ <Perlito5::Grammar.digits> \]
          { $MATCH->{capture} = Rul::Constant->new( constant => chr( Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.digits"}) ) ) }
        | c <Perlito5::Grammar.digits>
          { $MATCH->{capture} = Rul::Constant->new( constant => chr( Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.digits"}) ) ) }
        | <any>
          #  \e  \E
          { $MATCH->{capture} = Rul::SpecialChar->new( char => Perlito5::Match::flat($MATCH->{any}) ) }
        ]
    |   \.
        { $MATCH->{capture} = Rul::Perlito5::AST::Dot->new() }
    |   '['
        <rule> ']'
        { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule}) }

};

token rule_term {
    |
        # { say 'matching terms'; }
        <rule_terms>
        {
            $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule_terms})
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { $MATCH->{capture} = Rul::Constant->new( constant => Perlito5::Match::flat($MATCH->{any}) ) }
};

token quant_exp  {   \? | \* | \+  };

token greedy_exp {   \?  |  \+  |  ''  };

token quantifier {
    <Perlito5::Grammar::Space.opt_ws>
    <rule_term>
    <Perlito5::Grammar::Space.opt_ws>
    [
        <quant_exp> <greedy_exp>
        <Perlito5::Grammar::Space.opt_ws>
        { $MATCH->{capture} = Rul::Quantifier->new(
                term    => Perlito5::Match::flat($MATCH->{rule_term}),
                quant   => Perlito5::Match::flat($MATCH->{quant_exp}),
                greedy  => Perlito5::Match::flat($MATCH->{greedy_exp}),
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
    { $MATCH->{capture} = Rul::Concat->new( concat => Perlito5::Match::flat($MATCH->{concat_list}) ) }
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
    [ <.Perlito5::Grammar::Space.ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list_exp>
    {
        # say 'found Rule';
        $MATCH->{capture} = Rul::Or->new( or_list => Perlito5::Match::flat($MATCH->{or_list_exp}) )
    }
};

=begin

=head1 NAME

Perlito5::Grammar::Regex - Grammar for Perlito Grammar

=head1 SYNOPSIS

    my $match = Perlito5::Grammar::Regex->rule( $source, $pos );
    Perlito5::Match::flat($match);    # generated Regex AST

=head1 DESCRIPTION

This module generates a syntax tree for the Regex compiler.

This is not useful during runtime, and this module should never be loaded in the compiled programs.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

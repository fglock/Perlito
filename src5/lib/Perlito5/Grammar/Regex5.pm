package Perlito5::Grammar::Regex5;

use Perlito5::Grammar::Precedence;
use strict;
use warnings;

token any { . };

# TODO
token ws { '' };

# TODO
token string_code {
    [  \\ .
    |  \{  <.string_code> \}
    |  <!before \} > .
    ]+
};

token parsed_code {
    <.string_code>
    { $MATCH->{capture} = Perlito5::Match::flat($MATCH) }
};

token rule_term {
    |   '^'     { $MATCH->{capture} = 'beginning_of_line' }
    |   '$'     { $MATCH->{capture} = 'end_of_line' }
    |   '.'     { $MATCH->{capture} = Perlito5::Rul::Dot->new() }

    |   '(?'
        [   ':' <rule> ')'
            { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule}) }
        |   '=' <.ws> <rule> ')'
            { $MATCH->{capture} = Perlito5::Rul::Before->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
        |   '!' <.ws> <rule> ')'
            { $MATCH->{capture} = Perlito5::Rul::NotBefore->new( rule_exp => Perlito5::Match::flat($MATCH->{rule}) ) }
        |   '{'  <parsed_code>  '})'
            { $MATCH->{capture} = Perlito5::Rul::Block->new( closure => Perlito5::Match::flat($MATCH->{parsed_code}) ) }
        ]

    |   '(' <rule> ')'
        { $MATCH->{capture} = Perlito5::Rul::Subrule->new( metasyntax => Perlito5::Match::flat($MATCH->{rule}), captures => 1 ) }

    |   \\
        [
        | 'c' \[ <Perlito5::Grammar::Number.digits> \]
          { $MATCH->{capture} = Perlito5::Rul::Constant->new( constant => chr( Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number.digits"}) ) ) }
        | 'c' <Perlito5::Grammar::Number.digits>
          { $MATCH->{capture} = Perlito5::Rul::Constant->new( constant => chr( Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number.digits"}) ) ) }
        | <any>   #  \e  \E
          { $MATCH->{capture} = Perlito5::Rul::SpecialChar->new( char => Perlito5::Match::flat($MATCH->{any}) ) }
        ]

    |   <!before '(' | ')' | '[' | ']' | '+' | '?' | '\\' | '|' | '*' >
        <any>
         { $MATCH->{capture} = Perlito5::Rul::Constant->new( constant => Perlito5::Match::flat($MATCH->{any}) ) }
};

token quant_exp  {
    [
    | '?'
    | '*'
    | '+' 
    | '{' <Perlito5::Grammar::Number.digits> '}'
    | '{' <Perlito5::Grammar::Number.digits> ',' '}'
    | '{' <Perlito5::Grammar::Number.digits> ',' <Perlito5::Grammar::Number.digits> '}'
    ]
    [ '?' | '+' | '' ]
};

token quantifier {
    <.ws>? <rule_term> <.ws>?
    [   <quant_exp> <.ws>?
        { $MATCH->{capture} = Perlito5::Rul::Quantifier->new(
                term    => Perlito5::Match::flat($MATCH->{rule_term}),
                quant   => Perlito5::Match::flat($MATCH->{quant_exp}),
            )
        }
    |   { $MATCH->{capture} = Perlito5::Match::flat($MATCH->{rule_term}) }
    ]
};

token concat_list {
    <quantifier>
    [   <concat_list>
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{quantifier}), @{Perlito5::Match::flat($MATCH->{concat_list})} ] }
    |
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{quantifier}) ] }
    ]
    |   { $MATCH->{capture} = [] }
};

token concat_exp {
    <concat_list>
    {
        $arg = Perlito5::Match::flat($MATCH->{concat_list});
        if (@$arg < 1) {
            $MATCH->{capture} = "empty";
        }
        elsif (@$arg < 2) {
            ($MATCH->{capture}) = @$arg;
        }
        else {
            $MATCH->{capture} = Perlito5::Rul::Concat->new( concat => $arg )
        }
    }
};

token or_list_exp {
    <concat_exp>
    [   '|' <or_list_exp>
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{concat_exp}), @{Perlito5::Match::flat($MATCH->{or_list_exp})} ] }
    |
        { $MATCH->{capture} = [ Perlito5::Match::flat($MATCH->{concat_exp}) ] }
    ]
    |   { $MATCH->{capture} = [] }
};

token rule {
    <or_list_exp>
    {
        $arg = Perlito5::Match::flat($MATCH->{or_list_exp});
        if (@$arg < 1) {
            $MATCH->{capture} = "empty";
        }
        elsif (@$arg < 2) {
            ($MATCH->{capture}) = @$arg;
        }
        else {
            $MATCH->{capture} = Perlito5::Rul::Or->new( or_list => $arg )
        }
    }
};

=begin

=head1 NAME

Perlito5::Grammar::Regex5 - Grammar for Perl5 regex

=head1 SYNOPSIS

    my $match = Perlito5::Grammar::Regex5->rule( $source, $pos );
    Perlito5::Match::flat($match);    # generated Regex5 AST

=head1 DESCRIPTION

This module generates a syntax tree for the Regex5 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2014 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

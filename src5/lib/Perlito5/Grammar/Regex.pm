package Perlito5::Grammar::Regex;

use Perlito5::Precedence;


# Register the "token" keyword
#
# This is not a Perl5 word, but Perl6 - but it is useful inside the grammar compiler


token token {
    <Perlito5::Grammar.ident>  <.Perlito5::Grammar.opt_ws> \{
        <Perlito5::Grammar::Regex.rule>
    \}
    {
        #say 'Token was compiled into: ', ($MATCH->{"Perlito5::Grammar::Regex.rule"}->flat())->perl;
        my $source = $MATCH->{"Perlito5::Grammar.ident"}->flat()
            . '{ ' .
                'my $grammar = $_[0]; ' .
                'my $str     = $_[1]; ' .
                'my $pos     = $_[2]; ' .
                'my $MATCH = Perlito5::Match->new( str => $str, from => $pos, to => $pos, bool => 1 ); ' .
                '$MATCH->{"bool"} = ( ' .
                    $MATCH->{"Perlito5::Grammar::Regex.rule"}->flat()->emit_perl5() .
                '); ' .
                '$MATCH; '
            . '}';
        #say 'Intermediate code: ', $source;
        my $ast = Perlito5::Grammar->named_sub_def( $source, 0 );
        # say 'Intermediate ast: ', $ast->flat;
        $MATCH->{"capture"} = $ast->flat();
    }
};

token term_token {
    'token' <.Perlito5::Grammar.ws> <token>
                { $MATCH->{"capture"} = [ 'term', $MATCH->{"token"}->flat()       ] }
};

Perlito5::Precedence::add_term( 'token', sub { Perlito5::Grammar::Regex->term_token($_[0], $_[1]) } );


# this is the "grammar grammar"


token ws {  <.Perlito5::Grammar.ws>  };

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
    { $MATCH->{"capture"} = $MATCH->flat() }
};

token rule_terms {
    |   '<before'
        <.ws> <rule> \>
        { $MATCH->{"capture"} = Rul::Before->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   '<!before'
        <.ws> <rule> \>
        { $MATCH->{"capture"} = Rul::NotBefore->new( rule_exp => $MATCH->{"rule"}->flat() ) }
    |   \'
        <literal> \'
        { $MATCH->{"capture"} = Rul::Constant->new( constant => $MATCH->{"literal"}->flat() ) }
    |   \<
        [
            \.
            <metasyntax_exp>  \>
            { $MATCH->{"capture"} = Rul::Perlito5::AST::Subrule->new( metasyntax => $MATCH->{"metasyntax_exp"}->flat(), captures => 0 ) }
        |
            <metasyntax_exp>  \>
            { $MATCH->{"capture"} = Rul::Perlito5::AST::Subrule->new( metasyntax => $MATCH->{"metasyntax_exp"}->flat(), captures => 1 ) }
        ]
    |   \{
        <parsed_code>  \}
        { $MATCH->{"capture"} = Rul::Block->new( closure => $MATCH->{"parsed_code"}->flat() ) }
    |   \\
        [
        | c \[ <Perlito5::Grammar.digits> \]
          { $MATCH->{"capture"} = Rul::Constant->new( constant => chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) ) }
        | c <Perlito5::Grammar.digits>
          { $MATCH->{"capture"} = Rul::Constant->new( constant => chr( $MATCH->{"Perlito5::Grammar.digits"}->flat() ) ) }
        | <any>
          #  \e  \E
          { $MATCH->{"capture"} = Rul::SpecialChar->new( char => $MATCH->{"any"}->flat() ) }
        ]
    |   \.
        { $MATCH->{"capture"} = Rul::Perlito5::AST::Dot->new() }
    |   '['
        <rule> ']'
        { $MATCH->{"capture"} = $MATCH->{"rule"}->flat() }

};

token rule_term {
    |
        # { say 'matching terms'; }
        <rule_terms>
        {
            $MATCH->{"capture"} = $MATCH->{"rule_terms"}->flat()
        }
    |  <!before \] | \} | \) | \> | \: | \? | \+ | \* | \| | \& | \/ > <any>   # TODO - <...>* - optimize!
        { $MATCH->{"capture"} = Rul::Constant->new( constant => $MATCH->{"any"}->flat() ) }
};

token quant_exp  {   \? | \* | \+  };

token greedy_exp {   \?  |  \+  |  ''  };

token quantifier {
    <Perlito5::Grammar.opt_ws>
    <rule_term>
    <Perlito5::Grammar.opt_ws2>
    [
        <quant_exp> <greedy_exp>
        <Perlito5::Grammar.opt_ws3>
        { $MATCH->{"capture"} = Rul::Quantifier->new(
                term    => $MATCH->{"rule_term"}->flat(),
                quant   => $MATCH->{"quant_exp"}->flat(),
                greedy  => $MATCH->{"greedy_exp"}->flat(),
                ws1     => $MATCH->{"Perlito5::Grammar.opt_ws"}->flat(),
                ws2     => $MATCH->{"Perlito5::Grammar.opt_ws2"}->flat(),
                ws3     => $MATCH->{"Perlito5::Grammar.opt_ws3"}->flat(),
            )
        }
    |
        { $MATCH->{"capture"} = $MATCH->{"rule_term"}->flat() }
    ]
};

token concat_list {
    <quantifier>
    [
        <concat_list>
        { $MATCH->{"capture"} = [ $MATCH->{"quantifier"}->flat(), @{$MATCH->{"concat_list"}->flat()} ] }
    |
        { $MATCH->{"capture"} = [ $MATCH->{"quantifier"}->flat() ] }
    ]
    |
        { $MATCH->{"capture"} = [] }
};

token concat_exp {
    <concat_list>
    { $MATCH->{"capture"} = Rul::Concat->new( concat => $MATCH->{"concat_list"}->flat() ) }
};

token or_list_exp {
    <concat_exp>
    [
        '|'
        <or_list_exp>
        { $MATCH->{"capture"} = [ $MATCH->{"concat_exp"}->flat(), @{$MATCH->{"or_list_exp"}->flat()} ] }
    |
        { $MATCH->{"capture"} = [ $MATCH->{"concat_exp"}->flat() ] }
    ]
    |
        { $MATCH->{"capture"} = [] }
};

token rule {
    [ <.ws>? '|' | '' ]
    # { say 'trying M::G::Rule on ', $s }
    <or_list_exp>
    {
        # say 'found Rule';
        $MATCH->{"capture"} = Rul::Or->new( or_list => $MATCH->{"or_list_exp"}->flat() )
    }
};

=begin

=head1 NAME

Perlito5::Grammar::Regex - Grammar for Perlito Grammar

=head1 SYNOPSIS

    my $match = Perlito5::Grammar::Regex->rule( $source, $pos );
    $match->flat();    # generated Regex AST

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

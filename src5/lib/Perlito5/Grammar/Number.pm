
package Perlito5::Grammar::Number;
use strict;
use Perlito5::Grammar::Precedence;

token term_digit {
    | <Perlito5::Grammar::Number::val_octal>
        # 0123
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::val_octal"}) ]  }
    | <Perlito5::Grammar::Number::val_vstring>
        # 123.456.789
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::val_vstring"}) ]  }
    | <Perlito5::Grammar::Number::val_num>
        # 123.456
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::val_num"}) ]  }
    | <Perlito5::Grammar::Number::val_int>
        # 123
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::val_int"}) ]  }
};

Perlito5::Grammar::Precedence::add_term( $_  => \&term_digit )
    for '.', '0' .. '9';


sub digit {
    my $str = $_[0];
    my $pos = $_[1];
    $str->[$pos] ge '0' && $str->[$pos] le '9'
    ? {
        str  => $str,
        from => $pos,
        to   => $pos + 1,
      }
    : 0;
}

token exponent {
    [ 'e' | 'E' ]  [ '+' | '-' | '' ]  [ '_' | \d ]+
};

token val_num {
    [   \. \d [ '_' | \d]*    <.exponent>?    # .10 .10e10
    |      \d [ '_' | \d]*  [ <.exponent>  |   \. <!before \. > [ '_' | \d]*  <.exponent>? ]
    ]
    {
        my $s = Perlito5::Match::flat($MATCH);
        $s =~ s/_//g;
        $MATCH->{capture} = Perlito5::AST::Num->new( num => $s ) 
    }
};

token digits {
    \d+
};

token digits_underscore {
    \d [ '_' | \d]*
};

token val_octal {
    '0' [  ['x'|'X'] <.Perlito5::Grammar::word>+   # XXX test for hex digits
        |  ['b'|'B'] [ '_' | '0' | '1' ]+
        |  [ '_' | \d]+        # XXX test for octal digits
        ]
        { $MATCH->{capture} = Perlito5::AST::Int->new( int => oct(lc(Perlito5::Match::flat($MATCH))) ) }
};

token val_int {
    <.digits_underscore>
        {
            my $s = Perlito5::Match::flat($MATCH);
            $s =~ s/_//g;
            $MATCH->{capture} = Perlito5::AST::Int->new( int => $s )
        }
};

token val_vstring {
    <val_int> [ '.' <digits_underscore> ]+
    {
        my @parts = map { Perlito5::Match::flat($_) }
                        @{ $MATCH->{digits_underscore} };
        return if @parts < 2;
        $MATCH->{capture} = Perlito5::AST::Buf->new(
                    is_vstring => 1,
                    buf => join( '', map { chr($_) }
                                        $MATCH->{val_int}{capture}{int},
                                        @parts,
                               ),
               );
    }
};

token val_version {
    'v' <val_int> 
        <!before  <.Perlito5::Grammar::Space::opt_ws> \(  >
        [ '.' <digits_underscore> ]*
    {
        my @parts = map { Perlito5::Match::flat($_) }
                        @{ $MATCH->{digits_underscore} };
        $MATCH->{capture} = Perlito5::AST::Buf->new(
                    buf => join( '', map { chr($_) }
                                        $MATCH->{val_int}{capture}{int},
                                        @parts,
                               ),
               );
    }
};

1;

=begin

=head1 NAME

Perlito5::Grammar::Number - Parser and AST generator for Perlito

=head1 SYNOPSIS

    term_digit($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2013 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


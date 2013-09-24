
package Perlito5::Grammar::Number;
use strict;
use Perlito5::Grammar::Precedence;

Perlito5::Grammar::Precedence::add_term( '.' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '0' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '1' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '2' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '3' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '4' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '5' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '6' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '7' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '8' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( '9' => sub { Perlito5::Grammar::Number->term_digit( $_[0], $_[1] ) } );


token term_digit {
    | <Perlito5::Grammar::Number.val_octal>
        # 0123
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number.val_octal"}) ]  }
    | <Perlito5::Grammar::Number.val_num>
        # 123.456
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number.val_num"}) ]  }
    | <Perlito5::Grammar::Number.val_int>
        # 123
        { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number.val_int"}) ]  }
};

sub digit {
    substr( $_[1], $_[2], 1 ) =~ m/\d/
    ? {
        str  => $_[1],
        from => $_[2],
        to   => $_[2] + 1,
      }
    : 0;
}

token exponent {
    [ 'e' | 'E' ]  [ '+' | '-' | '' ]  [ '_' | \d ]+
};

token val_num {
    [
    |   \. \d [ '_' | \d]*    <.exponent>?    # .10 .10e10
    |      \d [ '_' | \d]*  [ <.exponent>  |   \. <!before \. > [ '_' | \d]*  <.exponent>? ]
    ]
    {
        my $s = Perlito5::Match::flat($MATCH);
        $s =~ s/_//g;
        $MATCH->{capture} = Perlito5::AST::Val::Num->new( num => $s ) 
    }
};

token digits {
    \d+
};

token digits_underscore {
    \d [ '_' | \d]*
};

token val_octal {
    '0' [  ['x'|'X'] <.Perlito5::Grammar.word>+   # XXX test for hex digits
        |  ['b'|'B'] [ '_' | '0' | '1' ]+
        |  [ '_' | \d]+        # XXX test for octal number
        ]
        { $MATCH->{capture} = Perlito5::AST::Val::Int->new( int => oct(lc(Perlito5::Match::flat($MATCH))) ) }
};

token val_int {
    <.digits_underscore>
        {
            my $s = Perlito5::Match::flat($MATCH);
            $s =~ s/_//g;
            $MATCH->{capture} = Perlito5::AST::Val::Int->new( int => $s )
        }
};

token val_version {
    # TODO - this should return a "VSTRING"
    ['v']? <.digits_underscore> [ '.' <.digits_underscore> [ '.' <.digits_underscore> ]? ]?
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


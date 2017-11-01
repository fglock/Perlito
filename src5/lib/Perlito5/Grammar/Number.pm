
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
    # | <Perlito5::Grammar::Number::val_int>
    #     # 123
    #     { $MATCH->{capture} = [ 'term', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Number::val_int"}) ]  }
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

sub octal_digit {
    my $str = $_[0];
    my $pos = $_[1];
    $str->[$pos] ge '0' && $str->[$pos] le '7'
    ? {
        str  => $str,
        from => $pos,
        to   => $pos + 1,
      }
    : 0;
}

sub hex_digit {
    my $str = $_[0];
    my $pos = $_[1];
    (  ($str->[$pos] ge '0' && $str->[$pos] le '9')
    || ($str->[$pos] ge 'A' && $str->[$pos] le 'F')
    || ($str->[$pos] ge 'a' && $str->[$pos] le 'f')
    )
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
    |      \d [ '_' | \d]*
                [ <.exponent>
                | \. <!before \. > [ '_' | \d]*  <.exponent>?
                |
                    {
                        my $s = Perlito5::Match::flat($MATCH);
                        $s =~ s/_//g;
                        $MATCH->{capture} = Perlito5::AST::Int->new( int => $s );
                        return $MATCH;
                    }
                ]
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

token hex_digits_underscore {
    [ '_' | <.Perlito5::Grammar::Number::hex_digit> ]+
};
token hex_digits_underscore2 {
    <.Perlito5::Grammar::Number::hex_digit> [ '_' | <.Perlito5::Grammar::Number::hex_digit> ]*
};
token hex_exponent {
    [ '+' | '-' | '' ]  <.Perlito5::Grammar::Number::hex_digits_underscore>
};

token val_octal {
    '0' [  ['x'|'X'] 

            [   # hexfloat
                [   \. <Perlito5::Grammar::Number::hex_digits_underscore2>+
                |   <Perlito5::Grammar::Number::hex_digits_underscore>+
                    [ \. <Perlito5::Grammar::Number::hex_digits_underscore2>?
                    | ''
                    ]
                ]
                [ 'p' | 'P' ]  <hex_exponent>
                {
                    my $h1 = $MATCH->{"Perlito5::Grammar::Number::hex_digits_underscore"};
                    my $h2 = $MATCH->{"Perlito5::Grammar::Number::hex_digits_underscore2"};
                    my $h3 = Perlito5::Match::flat($MATCH->{"hex_exponent"});

                    if ($h1 && @$h1) {
                        $h1 = Perlito5::Match::flat($h1->[0]);
                    }
                    else {
                        $h1 = 0;
                    }

                    if ($h2 && @$h2) {
                        $h2 = Perlito5::Match::flat($h2->[0]);
                    }
                    else {
                        $h2 = 0;
                    }

                    $h1 =~ s/_//g;
                    $h2 =~ s/_//g;
                    $h3 =~ s/_//g;
                    my $mul = 1;
                    my $sig = substr($h3, 0, 1);
                    if ( $sig eq '+' || $sig eq '-' ) {
                        $mul = $sig . '1';
                        $h3 = substr($h3, 1);
                    }
                    $h1 = hex($h1);
                    if (length($h2)) {
                        $h2 = hex($h2) / ( 16 ** length($h2) );
                    }
                    else {
                        $h2 = 0;
                    }
                    $h3 = hex($h3);
                    $MATCH->{capture} = Perlito5::AST::Num->new( num => ( $h1 + $h2 ) * (2 ** ( $mul * $h3)) );
                    return $MATCH;
                }
            |   <Perlito5::Grammar::Number::hex_digits_underscore>+
            ]

        |  ['b'|'B'] [ '_' | '0' | '1' ]+
        |  [ '_' | <.Perlito5::Grammar::Number::octal_digit> ]+
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
The Pugs Team.

=head1 COPYRIGHT

Copyright 2013 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


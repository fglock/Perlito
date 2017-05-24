use v5;
use strict;
use feature 'say';

say '1..2';
my %a;
$a{abc} = 3;
$a{xyz} = 4;

my @out;
while ( my ( $key, $value ) = each %a ) {
    push @out, ( $key, $value );
}

print 'not ' if "@out" ne "abc 3 xyz 4" && "@out" ne "xyz 4 abc 3";
say "ok 1 - while-each # @out";


sub ea {
    each %{$_[0]};
}

@out = ();
while ( my ( $key, $value ) = ea(\%a) ) {
    push @out, ( $key, $value );
}

print 'not ' if "@out" ne "abc 3 xyz 4" && "@out" ne "xyz 4 abc 3";
say "ok 2 - while-each # @out";


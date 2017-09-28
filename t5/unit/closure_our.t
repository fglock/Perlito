
use strict;
use feature 'say';

print "1..3\n";

my $c;

{
    package Testing;
    our $x = 123;
    $c = sub {$x};
}

if ( $c->() != 123 ) {
    print "not ";
}
say "ok 1";

# array

{
    package Testing;
    our @x = (20,25);
    $c = sub { \@x };
}

if ( scalar( @{ $c->() } ) != 2 ) {
    print "not ";
}
say "ok 2";

# hash

{
    package Testing;
    our %x = ( a => 20, b => 25 );
    $c = sub { \%x };
}

if ( scalar( @{[ %{ $c->() } ]} ) != 4 ) {
    print "not ";
}
say "ok 3";



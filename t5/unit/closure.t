
use strict;
use feature 'say';

print "1..3\n";

my $c;

{
    my $x = 123;
    $c = sub {$x};
}

if ( $c->() != 123 ) {
    print "not ";
}
say "ok 1";

# array

{
    my @x = (20,25);
    $c = sub { \@x };
}

if ( scalar( @{ $c->() } ) != 2 ) {
    print "not ";
}
say "ok 2";

# hash

{
    my %x = ( a => 20, b => 25 );
    $c = sub { \%x };
}

if ( scalar( @{[ %{ $c->() } ]} ) != 4 ) {
    print "not ";
}
say "ok 3";



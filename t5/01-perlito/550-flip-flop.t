use v5;
use strict;
use feature 'say';

say '1..2';

my @lines = ( "   - Foo", "01 - Bar", "1  - Baz", "   - Quux" );
my @result;

foreach (@lines) {
    if ( /0/ .. /1/ ) {
        push @result, $_;
    }
}
if ("@result" ne "01 - Bar") {
    print "not "
}
print "ok 1  # [@result]\n";


@result = ();

foreach (@lines) {
    if ( /0/ ... /1/ ) {
        push @result, $_;
    }
}
if ("@result" ne "01 - Bar 1  - Baz") {
    print "not "
}
print "ok 2  # [@result]\n";


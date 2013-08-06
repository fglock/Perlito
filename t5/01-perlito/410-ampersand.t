use feature 'say';
use strict;

say "1..15";

my $v = 0;
my $r = 0;
my $e = '';

sub with_proto () {
    if (@_) {
        $v += $_[0]
    }
    else {
        $v += 8
    }
}

sub no_proto {
    if (@_) {
        $v += $_[0]
    }
    else {
        $v += 8
    }
}

# sanity test with proto

$v = 3;
$r = with_proto + 4;
print "not " if $v != 11;
print "ok 1 - with_proto $v\n";
print "not " if $r != 15;
print "ok 2 - with_proto $r\n";

# sanity test without proto

$v = 3;
$r = no_proto + 4;
print "not " if $v != 7;
print "ok 3 - no_proto $v\n";
print "not " if $r != 7;
print "ok 4 - no_proto $r\n";


# ampersand with proto

$v = 3;
$r = &with_proto + 4;
print "not " if $v != 11;
print "ok 5 - with_proto $v\n";
print "not " if $r != 15;
print "ok 6 - with_proto $r\n";

# ampersand without proto

$v = 3;
$r = &no_proto + 4;
print "not " if $v != 11;
print "ok 7 - no_proto $v\n";
print "not " if $r != 15;
print "ok 8 - no_proto $r\n";


# ampersand with proto, parenthesis

$v = 3;
$r = &with_proto(4);
print "not " if $v != 7;
print "ok 9 - with_proto $v\n";
print "not " if $r != 7;
print "ok 10 - with_proto $r\n";

# ampersand without proto, with parenthesis

$v = 3;
$r = &no_proto(4);
print "not " if $v != 7;
print "ok 11 - no_proto $v\n";
print "not " if $r != 7;
print "ok 12 - no_proto $r\n";


# syntax with proto

$v = 3;
$r = 0;
eval ' $r = with_proto 4 ';
$e = $@;
print "not " if !$e;
print "ok 13 - syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";
print "not " if $v != 3;
print "ok 14 - with_proto $v\n";
print "not " if $r != 0;
print "ok 15 - with_proto $r\n";



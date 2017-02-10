use feature 'say';
use strict;

say "1..28";

my $v = 0;
my $r = 0;
my $e = '';

sub with_proto () {
    if (@_) {
        $v += $_[0];
    }
    else {
        $v += 8;
    }
}

sub no_proto {
    if (@_) {
        $v += $_[0];
    }
    else {
        $v += 8;
    }
}

sub with_proto_return_sub () {
    sub { $_[0] + 7 }
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
$r = no_proto +4;
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

# ampersand reference

$v = 3;
$r = \&with_proto;
$r = &$r(4);
print "not " if $v != 7;
print "ok 13 - with_proto $v\n";
print "not " if $r != 7;
print "ok 14 - with_proto $r\n";

# ampersand reference, no parenthesis

$v = 3;
$r = \&with_proto;
$r = &$r;
print "not " if $v != 11;
print "ok 15 - with_proto $v\n";
print "not " if $r != 11;
print "ok 16 - with_proto $r\n";

# ampersand reference, call with arrow

$v = 3;
$r = \&with_proto;
$r = $r->(4);
print "not " if $v != 7;
print "ok 17 - with_proto $v\n";
print "not " if $r != 7;
print "ok 18 - with_proto $r\n";

# ampersand string

$v = 3;
$r = \&{"with_proto"};
$r = $r->(4);
print "not " if $v != 7;
print "ok 19 - with_proto $v\n";
print "not " if $r != 7;
print "ok 20 - with_proto $r\n";

# ampersand string, no strict

{
    no strict;
    $v = 3;
    $r = &{"with_proto"}(4);
    print "not " if $r != 7;
    print "ok 21 - ampersand string - $r\n";
}

# ampersand string, no strict

{
    no strict;
    $v = 3;
    $r = &{"with_proto"};
    print "not " if $r != 11;
    print "ok 22 - ampersand string - $r\n";
}

# ampersand string, return sub

{
    no strict;
    $r = &{"with_proto_return_sub"}->(4);
    print "not " if $r != 11;
    print "ok 23 - with_proto_return_sub # $r\n";
}

# ampersand, return sub

$r = &with_proto_return_sub->(4);
print "not " if $r != 11;
print "ok 24 - with_proto_return_sub # $r\n";

# ampersand, default to @_

$v = 3;
@_ = (4);
$r = &with_proto;
print "not " if $v != 7;
print "ok 25 - with_proto $v\n";
print "not " if $r != 7;
print "ok 26 - with_proto $r\n";

# sanity test with proto attribute

sub with_proto_attr : prototype() {
    if (@_) {
        $v += $_[0];
    }
    else {
        $v += 8;
    }
}

$v = 3;
$r = with_proto_attr +4;
print "not " if $v != 11;
print "ok 27 - with_proto_attr $v\n";
print "not " if $r != 15;
print "ok 28 - with_proto_attr $r\n";


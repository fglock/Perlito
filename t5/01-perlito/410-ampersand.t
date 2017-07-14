use feature 'say';
use strict;

say "1..9";

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

# syntax with proto

$v = 3;
$r = 0;
eval ' $r = with_proto 4 ';
$e = $@;
print "not " if !$e;
print "ok 1  - syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";
print "not " if $v != 3;
print "ok 2  - with_proto $v\n";
print "not " if $r != 0;
print "ok 3  - with_proto $r\n";

# ampersand string

$v = 3;
eval ' $r = &{"with_proto"}(4) ';
$e = $@;
print "not " if !$e;
print "ok 4  - syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";

# ampersand string

$v = 3;
eval ' $r = &{"with_proto"} ';
$e = $@;
print "not " if !$e;
print "ok 5  - syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";

# ampersand string, no strict

{
    no strict;
    $v = 3;
    eval ' $r = &{"with_proto"}(4) ';
    $e = $@;
    print "not " if $e;
    print "ok 6  - not syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";
}

# ampersand string, no strict

{
    no strict;
    $v = 3;
    eval ' $r = &{"with_proto"} ';
    $e = $@;
    print "not " if $e;
    print "ok 7  - not syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";
}

# ampersand string

$v = 3;
eval ' $r = &{"with_proto"}->(4) ';
$e = $@;
print "not " if !$e;
print "ok 8  - syntax error - '" . ( $e ? substr( $e, 0, 30 ) : '' ) . "...' \n";

# plain &

$v = 3;
$r = 0;
&with_proto;
print "not " if $v != 11;
print "ok 9  - with_proto $v\n";


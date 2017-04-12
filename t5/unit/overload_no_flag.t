use strict;
use warnings;
use feature ("say");

say "1..2";

{ package S;
    no strict 'refs';
    *{'S::(""'} = sub { "123" };
    # *{'S::(('} = sub { };     # sets the overload flag
    # *{'S::()'} = sub { };     # sets the overload fallback flag
    # *{'S::()'} = \"fallback";
}

my $res;
my $s;
my $v = bless {}, "S";
$res = substr("$v", 0, 6);
print "not " if $res ne "S=HASH";
print "ok 1 - overload # $res\n";

$res = 0 + $v;
print "not " if $res eq "123";
print "ok 2 - overload fallback # $res\n";



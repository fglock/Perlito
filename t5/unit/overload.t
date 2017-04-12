use strict;
use warnings;
use feature ("say");

say "1..6";

{ package S;
    no strict 'refs';
    *{'S::(""'} = sub { "123" };
    *{'S::(('} = sub { };     # sets the overload flag
}

my $res;
my $s;
my $v = bless {}, "S";
$res = "$v";
print "not " if $res ne "123";
print "ok 1 - overload # $res\n";

$s = '(""';
$res = $v->$s;
print "not " if $res ne "123";
print "ok 2 - method call # $res\n";

# TODO - test if fallback is enabled
# $res = 0 + $v;
# print "not " if $res ne "123";
print "ok 3 - overload fallback # $res # TODO\n";


{ package N;
    no strict 'refs';
    *{'N::(0+'} = sub { 123 };
    *{'N::()'} = sub {};    # Make it findable via fetchmethod
    *{'N::()'} = \"fallback";
}

$v = bless {}, "N";
$res = 0 + $v;
print "not " if $res != 123;
print "ok 4 - overload # $res\n";

$s = '(0+';
$res = $v->$s;
print "not " if $res != 123;
print "ok 5 - method call # $res\n";

$res = "$v";
print "not " if $res ne "123";
print "ok 6 - overload fallback # $res\n";



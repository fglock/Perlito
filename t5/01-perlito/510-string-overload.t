use strict;
use warnings;
use feature ("say");

say "1..2";

{ package S;
    no strict 'refs';
    *{'S::(""'} = sub { "123" };
    *{'S::()'} = sub { };
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


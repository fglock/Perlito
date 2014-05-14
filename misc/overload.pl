use strict;
use warnings;

package S {
    no strict 'refs';
    *{'S::(""'} = sub { "123" };
    *{'S::()'} = sub { };
}

my $s;
my $v = bless {}, "S";
print $v,"\n";

$s = '(""';
print $v->$s,"\n";


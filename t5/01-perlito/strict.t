print "1..4\n";
eval('use strict;my $x = "ok 1";print $x,"\n"');
eval('my $x = "ok 2";print $x,"\n"');
eval('$x = "ok 3";print $x,"\n"');
eval('use strict;$x = "not ok 4";print $x,"\n"');
print "ok 4\n";

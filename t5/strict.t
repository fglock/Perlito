say "1..4";
eval('use strict;my $x = "ok 1";say $x');
eval('my $x = "ok 2";say $x');
eval('$x = "ok 3";say $x');
eval('use strict;$x = "not ok 4";say $x');
say "ok 4";


print "1..4\n";

my $v;

$v = this . that;
print "not " if $v ne "thisthat";
print "ok 1 - simple bareword  # $v\n";
 
$v = time . that;
print "not " if $v eq "timethat";
print "ok 2 - function name vs. bareword  # $v\n";
 
$v = default . that;
print "not " if $v ne "defaultthat";
print "ok 3 - feature-enabled function name  # $v\n";

$v = say . that;
print "not " if $v ne "saythat";
print "ok 4 - feature-enabled function name  # $v\n";


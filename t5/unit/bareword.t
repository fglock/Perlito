
print "1..7\n";

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

$v = ::this . that::;
print "not " if $v ne "::thisthat";
print "ok 5 - bareword with double colon # $v\n";

$v = ::this:: . that::those;
print "not " if $v ne "::thisthat::those";
print "ok 6 - bareword with double colon # $v\n";

$v = this . -that;
print "not " if $v ne "this-that";
print "ok 7 - bareword with dash # $v\n";
 

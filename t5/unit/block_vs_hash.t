
print "1..6\n";

sub v00 { {  1, 2  } }
sub v01 { {; 1, 2  } }
sub v02 { {  1; 2  } }
# sub v03 { {  1, 2; } }    # syntax error

sub v04 { {  1   } }
sub v05 { {  1,  } }
sub v06 { {  1;  } }

my $v;

$v = v00();
if (ref($v) ne 'HASH') {
    print "not ";
}
print "ok 1 # $v\n";

$v = v01();
if ($v ne '2') {
    print "not ";
}
print "ok 2 # $v\n";

$v = v02();
if ($v ne '2') {
    print "not ";
}
print "ok 3 # $v\n";

$v = v04();
if ($v ne '1') {
    print "not ";
}
print "ok 4 # $v\n";

$v = v05();
if (ref($v) ne 'HASH') {
    print "not ";
}
print "ok 5 # $v\n";

$v = v06();
if ($v ne '1') {
    print "not ";
}
print "ok 6 # $v\n";



print "1..2\n";

BEGIN {
    $_ = 123;
}

if ( $_ != 123 ) {
    print "not ";
}
print "ok 1  - init \$_ in BEGIN # $_\n";



BEGIN {
    $a = 123;
}

if ( $a != 123 ) {
    print "not ";
}
print "ok 2  - init \$a in BEGIN # $a\n";



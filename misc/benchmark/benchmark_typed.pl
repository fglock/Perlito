package Integer {}

my Integer $count = 0;

my Integer $i = 0;
while ( $i < 400 ) {
    my Integer $j = 0;
    while ( $j < 400 ) {
        my Integer $k = 0;
        while ( $k < 400 ) {
            $k = $k + 1;
            $count = $count + 1;
        }
        $j = $j + 1;
    }
    $i = $i + 1;
}

my $c = $count;
print "done $c\n";


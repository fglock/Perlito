package int {}

my int $count = 0;

my int $i = 0;
while ( $i < 400 ) {
    my int $j = 0;
    while ( $j < 400 ) {
        my int $k = 0;
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


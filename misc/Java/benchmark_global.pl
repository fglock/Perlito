
$i = 0;
while ( $i < 400 ) {
    $j = 0;
    while ( $j < 400 ) {
        $k = 0;
        while ( $k < 400 ) {
            $k = $k + 1;
            $count = $count + 1;
        }
        $j = $j + 1;
    }
    $i = $i + 1;
}

print "done $count\n";


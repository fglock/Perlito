
print "1..5\n";

my $c = 0;
my @a = ( 6, 7, 8 );
my %h = ( bb => 'bbv' );

my @expect = ( 6, 7, 8, 'bb', 'bbv' );

for my $i ( @a, %h ) {
    if ( $c == 1 ) {
        # print "set\n";
        push @a, 9;
        shift @a;
        $h{cc} = 'ccv';
    }

    if ($expect[$c] ne $i) {
        print "not ";
    }
    print "ok ", $c+1, " - $expect[$c] / $i\n";
    $c++;
}


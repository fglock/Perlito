
say '1..6';
my @a;
say 'ok 1 - create array';
$a[7][4] = 3;
say 'ok 2 - set element';
if ($a[7][4] != 3) {
    print 'not '
}
say 'ok 3 - fetch element # ', $a[7][4];

my $b;
say 'ok 4 - create scalar';
$b->[7][4] = 3;
say 'ok 5 - set element';
if ($b->[7][4] != 3) {
    print 'not '
}
say 'ok 6 - fetch element # ', $b->[7][4];

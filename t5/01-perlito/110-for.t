use feature 'say';

say '1..14';
my @a = ( 1, 2 );
for my $v (@a) {
    say 'ok ' . $v . ' - loop';
}

my $x = 123;
@a = ( 3, 6 );
for my $v (@a) {
    my $x = do { 3 };
    if ($x != 3) {
        print 'not '
    }
    say 'ok ' . $v . ' - for block';
    my @b = ( $v + 1, $v + 2 );
    for my $v (@b) {
        my $x = do { 3 };
        if ($x != 3) {
            print 'not '
        }
        say 'ok ' . $v . ' - inner for block';
    }
}

if ($x != 123) {
    print 'not '
}
say 'ok 9 - for block # ', $x;

$a[0] = 10;
$a[1] = 11;
for (@a) {
    say "ok $_ - default variable";
}

$a[0] = 12;
$a[1] = 13;
say "ok $_ - default variable in statement modifier"
    for @a;

@a = ( 1, 2 );
for my $v (@a) {
    $v++;
}
print "not " if $a[0] != 2 || $a[1] != 3;
say "ok 14 - loop variable is alias";


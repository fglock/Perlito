use feature 'say';

say '1..3';

$_ = [1,2,3];

for (@$_) {
    print "ok $_ - use \$_ both as the loop variable and in the loop expression\n";
}


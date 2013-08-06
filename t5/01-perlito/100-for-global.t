use feature 'say';
use strict;

say '1..9';

{
    my $v;
    $v = 7;
    my @a = ( 1, 2 );
    for $v (@a) {
        say 'ok ' . $v . ' - loop';
    }
    print "not " if $v != 7;
    print "ok 3 - leave loop\n";
}

{
    our $v;
    $v = 8;
    my @a = ( 4, 5 );
    for $v (@a) {
        say 'ok ' . $v . ' - loop';
    }
    print "not " if $v != 8;
    print "ok 6 - leave loop\n";
}

{
    $NEW::v = 12;
    my @a = ( 7, 8 );
    for $NEW::v (@a) {
        say 'ok ' . $NEW::v . ' - loop';
    }
    print "not " if $NEW::v != 12;
    print "ok 9 - leave loop\n";
}


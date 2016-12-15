use feature 'say';
use strict;

say '1..14';

{
    my $v;
    $v = 7;
    my @a = ( 1, 2 );
    my @subs;
    for $v (@a) {
        say 'ok ' . $v . ' - loop';
        push @subs, sub { $v };
    }
    print "not " if $v != 7;
    print "ok 3 - leave loop\n";

    print "not " if $subs[0]->() != 1 || $subs[1]->() != 2;
    print "ok 4 - capture # " . $subs[0]->() . ", " . $subs[1]->() . "\n";
}

{
    our $v;
    $v = 8;
    my @a = ( 5, 6 );
    for $v (@a) {
        say 'ok ' . $v . ' - loop';
    }
    print "not " if $v != 8;
    print "ok 7 - leave loop\n";
}

{
    $NEW::v = 12;
    my @a = ( 8, 9 );
    for $NEW::v (@a) {
        say 'ok ' . $NEW::v . ' - loop';
    }
    print "not " if $NEW::v != 12;
    print "ok 10 - leave loop\n";
}

{
    my $v;
    $v = 7;
    my @a = ( 11, 12 );
    my @subs;
    for my $v (@a) {
        say 'ok ' . $v . ' - loop';
        push @subs, sub { $v };
    }
    print "not " if $v != 7;
    print "ok 13 - leave loop\n";

    print "not " if $subs[0]->() != 11 || $subs[1]->() != 12;
    print "ok 14 - capture # " . $subs[0]->() . ", " . $subs[1]->() . "\n";
}



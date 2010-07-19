use v6;

class Main {
    say '1..24';
    
    say ( 0     ?? "not " !! "" ), "ok 1 - integer";
    say ( 1     ?? "" !! "not " ), "ok 2";

    say ( 0.0   ?? "not " !! "" ), "ok 3 - float";
    say ( 0.001 ?? "" !! "not " ), "ok 4";

    say ( "0"   ?? "not " !! "" ), "ok 5 - string";
    say ( ""    ?? "not " !! "" ), "ok 6";
    say ( "0.0" ?? "" !! "not " ), "ok 7";
    say ( "1"   ?? "" !! "not " ), "ok 8";
    say ( "aaa" ?? "" !! "not " ), "ok 9";

    say ( false ?? "not " !! "" ), "ok 10 - bool";
    say ( true  ?? "" !! "not " ), "ok 11";

    say ( []    ?? "not " !! "" ), "ok 12 - array";
    say ( ["x"] ?? "" !! "not " ), "ok 13";
    do {
        my @a;
        say ( @a    ?? "not " !! "" ), "ok 14";
        @a = [];
        say ( @a    ?? "" !! "not " ), "ok 15";
        @a = ["aaa"];
        say ( @a    ?? "" !! "not " ), "ok 16";
    }

    do {
        my $b;
        say ( $b    ?? "not " !! "" ), "ok 17 - scalar";
        $b = [];
        say ( $b    ?? "not " !! "" ), "ok 18";
        $b = ["aaa"];
        say ( $b    ?? "" !! "not " ), "ok 19";
    }

    do {
        my $b;
        say ( (@($b)) ?? "not " !! "" ), "ok 20 - scalar";
        $b = [];
        say ( (@($b)) ?? "not " !! "" ), "ok 21";
        $b = ["aaa"];
        say ( (@($b)) ?? "" !! "not " ), "ok 22";
    }

    say ( Main.new()  ?? "" !! "not " ), "ok 23 - object";

    say ( undef ?? "not " !! "" ), "ok 24 - undef";
}


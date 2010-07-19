use v6;

class Main {
    say '1..23';
    
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
        @a = ["aaa"];
        say ( @a    ?? "" !! "not " ), "ok 15";
    }

    do {
        my $b;
        say ( $b    ?? "not " !! "" ), "ok 16 - scalar";
        $b = [];
        say ( $b    ?? "not " !! "" ), "ok 17";
        $b = ["aaa"];
        say ( $b    ?? "" !! "not " ), "ok 18";
    }

    do {
        my $b;
        say ( (@($b)) ?? "not " !! "" ), "ok 19 - scalar";
        $b = [];
        say ( (@($b)) ?? "not " !! "" ), "ok 20";
        $b = ["aaa"];
        say ( (@($b)) ?? "" !! "not " ), "ok 21";
    }

    say ( Main.new()  ?? "" !! "not " ), "ok 22 - object";

    say ( undef ?? "not " !! "" ), "ok 23 - undef";

    do {
        # TODO
        my @a = [];
        say ( @a    ?? "# " !! "# not " ), "ok 24";
    }
}


use v6;

class Main {
    say '1..30';
    
    say ( 0     ?? "not " !! "" ), "ok 1 - integer";
    say ( 1     ?? "" !! "not " ), "ok 2";

    say ( 0.0   ?? "not " !! "" ), "ok 3 - float";
    say ( 0.001 ?? "" !! "not " ), "ok 4";

    say ( "0"   ?? "not " !! "" ), "ok 5 - string";
    say ( ""    ?? "not " !! "" ), "ok 6";
    say ( "0.0" ?? "" !! "not " ), "ok 7";
    say ( "1"   ?? "" !! "not " ), "ok 8";
    say ( "aaa" ?? "" !! "not " ), "ok 9";

    say ( False ?? "not " !! "" ), "ok 10 - bool";
    say ( True  ?? "" !! "not " ), "ok 11";

    say ( []    ?? "not " !! "" ), "ok 12 - array";
    say ( ["x"] ?? "" !! "not " ), "ok 13";
    do {
        my @a;
        say ( @a    ?? "not " !! "" ), "ok 14";
        @a = ["aaa"];
        say ( @a    ?? "" !! "not " ), "ok 15";
    }

    {
        my $b;
        say ( $b    ?? "not " !! "" ), "ok 16 - scalar";
        $b = [];
        say ( $b    ?? "not " !! "" ), "ok 17";
        $b = ["aaa"];
        say ( $b    ?? "" !! "not " ), "ok 18";
    }

    {
        my $b;
        say ( (@($b)) ?? "not " !! "" ), "ok 19 - scalar";
        $b = [];
        say ( (@($b)) ?? "not " !! "" ), "ok 20";
        $b = ["aaa"];
        say ( (@($b)) ?? "" !! "not " ), "ok 21";
    }

    say ( Main.new()  ?? "" !! "not " ), "ok 22 - object";

    say ( undef ?? "not " !! "" ), "ok 23 - undef";

    {
        # TODO
        my @a = [];  # element #0 is an array
        say ( @a    ?? "# TODO " !! "# TODO not " ); 
        say "ok 24";
    }

    say (( 10 || 20 ) == 10 ?? "" !! "not "), "ok 25";
    say (( 10 && 20 ) == 20 ?? "" !! "not "), "ok 26";
    say ((  0 || 20 ) == 20 ?? "" !! "not "), "ok 27";
    say ((  0 && 20 ) ==  0 ?? "" !! "not "), "ok 28";
    say (( "" || "x" ) eq "x" ?? "" !! "not "), "ok 29";
    say (( "" && "x" ) eq ""  ?? "" !! "not "), "ok 30";
}


use v5;
use feature 'say';
use strict;

package Main;

sub new {
    bless {}, 'Main';
}

    say '1..32';
    
    say +( 0     ? "not " : "" ), "ok 1 - integer";
    say +( 1     ? "" : "not " ), "ok 2";

    say +( 0.0   ? "not " : "" ), "ok 3 - float";
    say +( 0.001 ? "" : "not " ), "ok 4";

    say +( "0"   ? "not " : "" ), "ok 5 - string";
    say +( ""    ? "not " : "" ), "ok 6";
    say +( "0.0" ? "" : "not " ), "ok 7";
    say +( "1"   ? "" : "not " ), "ok 8";
    say +( "aaa" ? "" : "not " ), "ok 9";

    say +( []    ? "" : "not " ), "ok 10 - array";
    say +( ["x"] ? "" : "not " ), "ok 11";
    do {
        my @a;
        say +( @a    ? "not " : "" ), "ok 12";
        @a = ["aaa"];
        say +( @a    ? "" : "not " ), "ok 13";
    };

    {
        my $b;
        say +( $b    ? "not " : "" ), "ok 14 - scalar";
        $b = [];
        say +( $b    ? "" : "not " ), "ok 15";
        $b = ["aaa"];
        say +( $b    ? "" : "not " ), "ok 16";
    }

    {
        my $b;
        # "Can't use an undefined value as an ARRAY reference"
        # say +( (@{$b}) ? "not " : "" ), "ok 19 - scalar";

        $b = [];
        say +( (@{$b}) ? "not " : "" ), "ok 17";
        $b = ["aaa"];
        say +( (@{$b}) ? "" : "not " ), "ok 18";
    }

    say +( Main->new()  ? "" : "not " ), "ok 19 - object";

    say +( undef() ? "not " : "" ), "ok 20 - undef";

    {
        my @a = [];  # element #0 is an array
        print ( @a    ? "" : "not " ); 
        say "ok 21";
    }

    say +(( 10 || 20 ) == 10 ? "" : "not "), "ok 22";
    say +(( 10 && 20 ) == 20 ? "" : "not "), "ok 23";
    say +((  0 || 20 ) == 20 ? "" : "not "), "ok 24";
    say +((  0 && 20 ) ==  0 ? "" : "not "), "ok 25";
    say +(( "" || "x" ) eq "x" ? "" : "not "), "ok 26";
    say +(( "" && "x" ) eq ""  ? "" : "not "), "ok 27";

    say "ok 28" or die "not ok 28";

    my $h = {}; 
    say +( $h ? "" : "not " ), "ok 29 - hash in scalar";
    $h->{'x'} = 0;
    say +( $h ? "" : "not " ), "ok 30";

    my %h; 
    say +( %h ? "not " : "" ), "ok 31 - hash";
    $h{'x'} = 0;
    say +( %h ? "" : "not " ), "ok 32";


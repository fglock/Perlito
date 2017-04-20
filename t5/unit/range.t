use v5;
use strict;
use feature 'say';

package Main;
say '1..103';

my $test = 1;

sub is_count {
    my ( $res, $expect ) = @_;
    my $count = scalar(@$res);
    print "not " if $expect != $count;
    say "ok $test # expect count $expect, got [$count]";
    $test++;
}

sub is_last {
    my ( $res, $expect ) = @_;
    my $last = @$res ? $res->[-1] : "???";
    print "not " if $expect ne $last;
    say "ok $test # expect last $expect, got [$last]";
    $test++;
}

my @v;

@v = 0 .. 4;
is_count( \@v, 5 );
is_last( \@v, 4 );
@v = 4 .. 0;
is_count( \@v, 0 );

@v = '0' .. '4';
is_count( \@v, 5 );
@v = '4' .. '0';
is_count( \@v, 0 );

@v = "" .. "AZ";
is_count( \@v, 1 );
is_last( \@v, "" );

@v = "A" .. "AZ";    #  ABCDEFGHIJK...XYZAAABACA...AYAZ
is_count( \@v, 52 );
is_last( \@v, "AZ" );

@v = "B" .. "AZ";    #  BCDEFGHIJK...XYZAAABACA...AYAZ
is_count( \@v, 51 );
is_last( \@v, "AZ" );

@v = "0" .. "AZ";    #  01234567891011...979899
is_count( \@v, 100 );
is_last( \@v, "99" );

@v = 0 .. "AZ";      #  0
is_count( \@v, 1 );
is_last( \@v, "0" );

@v = "A" .. 10;
is_count( \@v, 11 );
is_last( \@v, "10" );

@v = "000" .. "AZ";
is_count( \@v, 0 );

@v = "5.3" .. 100;
is_count( \@v, 96 );
is_last( \@v, "100" );

@v = "5.3" .. "100";
is_count( \@v, 96 );
is_last( \@v, "100" );

@v = "5.3" .. "zzz";
is_count( \@v, 1 );
is_last( \@v, "5.3" );

@v = "5.3" .. "100.0";
is_count( \@v, 96 );
is_last( \@v, "100" );

@v = "5.3" .. "0";
is_count( \@v, 0 );

@v = "A-1" .. "A-9";
is_count( \@v, 1 );
is_last( \@v, "A-1" );

@v = "A-1" .. "A-";
is_count( \@v, 0 );

@v = "1-1" .. "1-9";
is_count( \@v, 1 );
is_last( \@v, "1-1" );

@v = " 1" .. " 9";
is_count( \@v, 9 );
is_last( \@v, "9" );

@v = "1 " .. "9 ";
is_count( \@v, 9 );
is_last( \@v, "9" );

@v = " 1 " .. " 9 ";
is_count( \@v, 9 );
is_last( \@v, "9" );

@v = "1 A" .. "9 A";
is_count( \@v, 1 );
is_last( \@v, "1 A" );

@v = -3 .. 3;
is_count( \@v, 7 );
is_last( \@v, 3 );

@v = "-3" .. "3";
is_count( \@v, 7 );
is_last( \@v, 3 );

@v = "-3" .. "A";
is_count( \@v, 0 );

@v = "3" .. "A";
is_count( \@v, 7 );
is_last( \@v, 9 );

@v = "-3" .. "0";
is_count( \@v, 4 );
is_last( \@v, 0 );

@v = "." .. "3";
is_count( \@v, 1 );
is_last( \@v, "." );

@v = "- 3" .. "+ 3";
is_count( \@v, 1 );
is_last( \@v, "- 3" );

@v = ( 2.18 .. 3.14 );
is_count( \@v, 2 );
is_last( \@v, 3 );

@v = ( "1_002.18" .. "1_003.14" );
is_count( \@v, 1 );
is_last( \@v, "1_002.18" );

@v = ( "1_002" .. "1_003" );
is_count( \@v, 1 );
is_last( \@v, "1_002" );

@v = "+3" .. "+8";
is_count( \@v, 6 );
is_last( \@v, 8 );

@v = "a0a" .. "a0c";
is_count( \@v, 1 );
is_last( \@v, "a0a" );

my $last = "???";
my $count = 0;
local $@ = "";
eval {
    for ( 0 .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 2;
    }
    1;
}
  and print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if substr($@,0,6) ne "Range "; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "???"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 0; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
eval {
    for ( "900" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  and print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if substr($@,0,6) ne "Range "; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "???"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 0; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
eval {
    for ( "900" .. "Nan" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  and print "not ";
print "ok $test - 'Nan'\n";
$test++;
print "not " if substr($@,0,6) ne "Range "; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "???"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 0; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
eval {
    for ( "0" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  or  print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if $@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "999"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 1000; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
eval {
    for ( "0E0" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  or  print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if $@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "0E0"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 1; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
print qq{# "1" .. "Inf" \n};
eval {
    for ( "1" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  and  print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if !$@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "???"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 0; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
print qq{# "0.0" .. "Inf" \n};
eval {
    for ( "0.0" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  or   print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if $@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "0.0"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 1; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
print qq{# "0.1" .. "Inf" \n};
eval {
    for ( "0.1" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  or   print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if $@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "0.1"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 1; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
print qq{# Ine" .. "Inf" \n};
eval {
    for ( "Ine" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 2;
    }
    1;
}
  or  print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if $@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "Inf"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 2; print "ok $test - count # $count\n";
$test++;

$last = "???";
$count = 0;
local $@ = "";
print qq{# " 0" .. "Inf" \n};
eval {
    for ( " 0" .. "Inf" ) {
        # print "# '$_'\n";
        $last = $_;
        $count++;
        last if $_ > 1010;
    }
    1;
}
  and print "not ";
print "ok $test - 'Inf'\n";
$test++;
print "not " if !$@; print "ok $test - error message # $@\n";
$test++;
print "not " if $last ne "???"; print "ok $test - last value # $last\n";
$test++;
print "not " if $count != 0; print "ok $test - count # $count\n";
$test++;



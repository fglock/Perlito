use v5;
use strict;
use feature 'say';

package Main;
say '1..48';

my $test = 1;

sub is_count {
    my ($res, $expect) = @_;
    my $count = scalar(@$res);
    print "not " if $expect != $count;
    say "ok $test # expect count $expect, got [$count]";
    $test++;
}

sub is_last {
    my ($res, $expect) = @_;
    my $last = @$res ? $res->[-1] : "???";
    print "not " if $expect ne $last;
    say "ok $test # expect last $expect, got [$last]";
    $test++;
}


my @v;

@v = 0..4;
is_count(\@v, 5);
is_last(\@v, 4);
@v = 4..0;
is_count(\@v, 0);

@v = '0'..'4';
is_count(\@v, 5);
@v = '4'..'0';
is_count(\@v, 0);

@v = "A".."AZ";
  #  ABCDEFGHIJK...XYZAAABACA...AYAZ
is_count(\@v, 52);
is_last(\@v, "AZ");

@v = "B".."AZ";
  #  BCDEFGHIJK...XYZAAABACA...AYAZ
is_count(\@v, 51);
is_last(\@v, "AZ");

@v = "0".."AZ";
  #  01234567891011...979899
is_count(\@v, 100);
is_last(\@v, "99");

@v = 0.."AZ";
  #  0
is_count(\@v, 1);
is_last(\@v, "0");

@v = "A"..10;
is_count(\@v, 11);
is_last(\@v, "10");

@v = "000".."AZ";
is_count(\@v, 0);


@v = "5.3"..100;
is_count(\@v, 96);
is_last(\@v, "100");

@v = "5.3".."100";
is_count(\@v, 96);
is_last(\@v, "100");

@v = "5.3".."zzz";
is_count(\@v, 1);
is_last(\@v, "5.3");

@v = "5.3".."100.0";
is_count(\@v, 96);
is_last(\@v, "100");

@v = "5.3".."0";
is_count(\@v, 0);

@v = "A-1".."A-9";
is_count(\@v, 1);
is_last(\@v, "A-1");

@v = "1-1".."1-9";
is_count(\@v, 1);
is_last(\@v, "1-1");

@v = " 1".." 9";
is_count(\@v, 9);
is_last(\@v, "9");

@v = "1 ".."9 ";
is_count(\@v, 9);
is_last(\@v, "9");

@v = " 1 ".." 9 ";
is_count(\@v, 9);
is_last(\@v, "9");

@v = "1 A".."9 A";
is_count(\@v, 1);
is_last(\@v, "1 A");

@v = -3..3;
is_count(\@v, 7);
is_last(\@v, 3);

@v = "-3".."3";
is_count(\@v, 7);
is_last(\@v, 3);

@v = "-3".."A";
is_count(\@v, 0);

@v = "3".."A";
is_count(\@v, 7);
is_last(\@v, 9);

@v = "-3".."0";
is_count(\@v, 4);
is_last(\@v, 0);

@v = (2.18 .. 3.14);
is_count(\@v, 2);
is_last(\@v, 3);


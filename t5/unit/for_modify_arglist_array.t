
print "1..5\n";

my $res;
my $x;

my $c = 0;
my @a = ( 6, 7, 8 );

my @expect = ( 6, 7, 8 );

for my $i ( @a, %h ) {
    if ( $c == 1 ) {
        # print "set\n";
        push @a, 9;
        shift @a;
    }

    if ($expect[$c] ne $i) {
        print "not ";
    }
    print "ok ", $c+1, " - $expect[$c] / $i\n";
    $c++;
}

$res = "";
@x = (1..5); for (@x) { $res .= "$_,"; push @x, "123" if @x < 10; }
print "not " if $res ne "1,2,3,4,5,123,123,123,123,123,";
print "ok 4 - $res\n";

$res = "";
@x = (1..5); for (@x) { $res .= "$_,"; shift @x }
print "not " if $res ne "1,3,5,";
print "ok 5 - $res\n";

# # Use of freed value in iteration
# $res = "";
# @a = ( 6, 7, 8 );
# @x = ( 3, 4, 5 );
# for (@a, @x) { $res .= "$_,"; shift @x }
# print "not " if $res ne "1,3,5,";
# print "ok 6 - $res\n";


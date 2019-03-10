
# use Data::Dumper;

print "1..18\n";

package Baz;
sub foo { shift @ARGV }

my $v;

@ARGV = ( "yy", "zz" );
my %h = (
    aa       => 123,
    bb       => 456,
    foo    => 789,
    'pop'    => 910,
    '+foo' => 101,
    '-foo' => 203,
    time     => 305,
    yy       => 444,
);

# print STDERR Dumper \%h;

$v = $ARGV[0];
print "not " if $v ne "yy";
print "ok 1 - foo is bareword when used as key in hash constructor # $v\n";

$v = $h{ +foo };
print "not " if $v ne "444";
print "ok 2 - +foo is not bareword when used as hash index # $v\n";

$v = $h{-foo};
print "not " if $v ne "203";
print "ok 3 - -foo is bareword when used as hash index # $v\n";

$v = $h{foo};
print "not " if $v ne "789";
print "ok 4 - foo is bareword when used as hash index # $v\n";

@ARGV = ( "yy", "zz" );
my %j = ( yy => 777, zz => 888, );
$j{time}     = 305;
$j{foo}    = 406;
$j{ +foo } = 507;
$j{-foo}   = 608;

# print STDERR Dumper \%j;

$v = $ARGV[0];
print "not " if $v ne "zz";
print "ok 5 - +foo is not bareword when used as hash index # $v\n";

$v = $j{ +foo };
print "not " if $v ne "888";
print "ok 6 - +foo is not bareword when used as hash index # $v\n";

@ARGV = ( "yy", "zz" );
my %h = (
    yy       => 444,
    zz       => 555,
    '+foo' => 678,
    '-foo' => 912,
    +foo   => 101,
    -foo   => 203,
);
# use Data::Dumper;
# print STDERR Dumper \%h;

$v = $ARGV[0];
print "not " if $v ne "yy";
print "ok 7 - foo  # $v\n";

$v = $h{ +foo };
print "not " if $v ne "444";
print "ok 8 - +foo is not bareword # $v\n";

$v = $h{-foo};
print "not " if $v ne "203";
print "ok 9 - -foo  # $v\n";

$v = $h{'foo'};
print "not " if $v ne "101";
print "ok 10 - +foo is bareword before '=>' # $v\n";

$v = $h{'+foo'};
print "not " if $v ne "678";
print "ok 11 - +foo as string # $v\n";

$v = $h{'-foo'};
print "not " if $v ne "203";
print "ok 12 - -foo as string # $v\n";

@ARGV = ( "yy", "zz", "aa", "bb", "cc" );
my %h = (
    yy       => 444,
    zz       => 555,
    aa       => 665,
    bb       => 774,
    cc       => 883,
    '+Baz::foo' => 678,
    '-Baz::foo' => 912,
    +Baz::foo   => 101,
    -Baz::foo   => 203,
    'Baz::foo'  => 404,
    '-aa'     => 123,
    '-bb'     => 456,
    '-cc'     => 789,
);
# use Data::Dumper;
# print STDERR Dumper \%h;

$v = $ARGV[0];
print "not " if $v ne "aa";
print "ok 13 - Baz::foo  # $v\n";

$v = $h{ +Baz::foo };
print "not " if $v ne "665";
print "ok 14 - +Baz::foo is not bareword # $v\n";

$v = $h{-Baz::foo};
print "not " if $v ne "456";
print "ok 15 - -Baz::foo  # $v\n";

$v = $h{'Baz::foo'};
print "not " if $v ne "404";
print "ok 16 - +Baz::foo is bareword before '=>' # $v\n";

$v = $h{'+Baz::foo'};
print "not " if $v ne "678";
print "ok 17 - +Baz::foo as string # $v\n";

$v = $h{'-Baz::foo'};
print "not " if $v ne "912";
print "ok 18 - -Baz::foo as string # $v\n";



print "1..12\n";
my $v;

{
    @ARGV = ("yy");
    my %h = ( +shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "shift";
    print "ok 1 - +shift is bareword before '=>'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h = ( -shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "-shift";
    print "ok 2 - -shift is bareword before '=>'; concatenates '-'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h = ( shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "shift";
    print "ok 3 - shift is bareword before '=>'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h = ( +CORE::shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "yy";
    print "ok 4 - +CORE::shift is function call before '=>'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h = ( -CORE::shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "-yy";
    print "ok 5 - -CORE::shift is function call before '=>'; concatenates '-'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h = ( CORE::shift => 101, );
    ($v) = keys %h;
    print "not " if $v ne "yy";
    print "ok 6 - CORE::shift is function call before '=>'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{ +shift } = 101;
    ($v) = keys %h;
    print "not " if $v ne "yy";
    print "ok 7 - +shift is function call in hash index  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{-shift} = 101;
    ($v) = keys %h;
    print "not " if $v ne "-shift";
    print "ok 8 - -shift is bareword in hash index; concatenates '-'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{shift} = 101;
    ($v) = keys %h;
    print "not " if $v ne "shift";
    print "ok 9 - shift is bareword call in hash index  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{ +CORE::shift } = 101;
    ($v) = keys %h;
    print "not " if $v ne "yy";
    print "ok 10 - +CORE::shift is function call in hash index  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{ -CORE::shift } = 101;
    ($v) = keys %h;
    print "not " if $v ne "-yy";
    print "ok 11 - -CORE::shift is function call in hash index; concatenates '-'  # $v\n";
}
{
    @ARGV = ("yy");
    my %h;
    $h{ CORE::shift } = 101;
    ($v) = keys %h;
    print "not " if $v ne "yy";
    print "ok 12 - CORE::shift is function call in hash index  # $v\n";
}


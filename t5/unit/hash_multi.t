
no warnings;

print "1..4\n";
my $k = join( $;, "a", "b" );
my $v;

{
    my %h;
    $h{$k} = 101;
    ($v) = keys %h;
    print "not " if $v ne $k;
    print "ok 1 - simple key  # $v\n";
}
{
    my %h;
    $h{ qw{ a b } } = 101;
    ($v) = keys %h;
    print "not " if $v ne $k;
    print "ok 2 - multi key  # $v\n";
}
{
    my %h;
    $h{ a => "b" } = 101;
    ($v) = keys %h;
    print "not " if $v ne $k;
    print "ok 3 - multi key with '=>' # $v\n";
}
{
    my %h;
    $h{ ( a => "b" ) } = 101;
    ($v) = keys %h;
    print "not " if $v ne $k;
    print "ok 4 - multi key with '()' # $v\n";
}


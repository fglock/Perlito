use feature "say";
use strict;

say "1..4";

# See also: http://www.perlmonks.org/?node_id=790129
#   "Mini-Tutorial: Scalar vs List Assignment Operator"

{
    # A list assignment in scalar context returns the number of elements on the right hand side:
    my $count = scalar( my ( $hello, $there, $world ) = ( 7, 8 ) );
    print "not " if $count != 2;
    say "ok 1 # $count";
}

{
    my @list = ( my ( $hello, $there, $world ) = ( 7, 8 ) );
    print "not " if scalar(@list) != 3;
    say "ok 2 # ", scalar(@list);
}

{
    my @list = ( my ( $hello, $there, @world ) = ( 7 .. 15 ) );
    print "not " if scalar(@list) != 9;
    say "ok 3 # ", scalar(@list);
    print "not " if scalar(@world) != 7;
    say "ok 4 # ", scalar(@world);
}


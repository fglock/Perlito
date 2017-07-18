use feature 'say';
use strict;

say '1..8';

sub ok {
    my ( $ok, $msg ) = @_;
    if ( !$ok ) {
        print "not ";
    }
    say "ok $msg";
}

my $want;
my @a;
my $x;
my @x;

sub res {
    $want = wantarray;
    @a;
}

@a = ( 7, 8 );
$x = sub {
    my @x = do {
        return do { 1; res(); }
    };
    3;
  }
  ->();
ok( defined $x && $x == 2, '1 - return do { ; } receives caller scalar context' );
print "# want ", ( $want ? $want : defined $want ? $want : "undef" ), "\n";
ok( defined($want) && $want eq "", '2 - want is ""' );

@x = sub {
    my $x = do {
        return do { 1; res(); }
    };
    3;
  }
  ->();
ok( "@x" eq "7 8", '3 - return do { ; } receives caller list context' );
print "# want ", ( $want ? $want : defined $want ? $want : "undef" ), "\n";
ok( defined($want) && $want eq "1", '4 - want is "1"' );

@a = ( 7, 8 );
$x = sub {
    do {
        my @v = do { 1; return res(); }
    };
    3;
  }
  ->();
ok( defined $x && $x == 2, '5 - return do { ; } receives caller scalar context' );
print "# want ", ( $want ? $want : defined $want ? $want : "undef" ), "\n";
ok( defined($want) && $want eq "", '5 - want is ""' );

@x = sub {
    do {
        my $v = do { 1; return res(); }
    };
    3;
  }
  ->();
ok( "@x" eq "7 8", '7 - return do { ; } receives caller list context' );
print "# want ", ( $want ? $want : defined $want ? $want : "undef" ), "\n";
ok( defined($want) && $want eq "1", '8 - want is "1"' );


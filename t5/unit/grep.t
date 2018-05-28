use feature 'say';
use strict;

print "1..3\n";

my @a;
my @y;
my $x;

@a = ( qw/ryba lufa ryba/ );
$x = grep { $_ eq 'ryba' } @a;
print 'not ' unless $x == 2;
say "ok 1  - simple grep {eq} in scalar context works: [ @a ]";

@a = ( qw/ryba lufa ryba koza/ );
$x = grep { $_ =~ /[bz]a$/ } @a;
print 'not ' unless $x == 3;
say "ok 2  - simple grep {=~} in scalar context works: [ @a ]";

@a = ( qw/ryba lufa ryba/ );
@y = grep { $_ eq 'ryba' } @a;
print 'not ' unless @y == 2 and $y[0] eq 'ryba' and $y[1] eq 'ryba';
say "ok 3  - simple grep {eq} in list context works: [ @a => @y ]";



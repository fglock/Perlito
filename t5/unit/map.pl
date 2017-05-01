use feature 'say';
use strict;

print "1..4\n";

my @a;
my @y;
my $x;

@a = ( qw/ryb luf ryb/ );
@y = map { $_ . 'a' } @a;
print 'not ' unless @y == 3 and $y[0] eq 'ryba' and $y[1] eq 'lufa';
say "ok 1  - simple map {.} in list context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
$x = map { $_ . 'a' } @a;
print 'not ' unless $x == 3;
say "ok 2  - simple map {.} in scalar context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
@y = map { $_ =~ /^ry/ ? $_ . 'a' : () } @a;
print 'not ' unless @y == 2 and $y[0] eq 'ryba' and $y[1] eq 'ryba';
say "ok 3  - map { ? . : () } in list context works: [ @a => @y ]";

@a = ( qw/ryb luf ryb/ );
$x = map { $_ =~ /^ry/ ? $_ . 'a' : () } @a;
print 'not ' unless $x == 2;
say "ok 4  - map { ? . : () } in scalar context works: [ @a => @y ]";



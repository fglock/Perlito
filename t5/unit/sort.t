use feature 'say';
use strict;

print "1..3\n";

my @a;
my @y;
my $x;

@a = qw/ryba lufa koza/;
@y = sort { $a cmp $b } @a;
print 'not ' unless $y[0] eq 'koza' and $y[1] eq 'lufa' and $y[2] eq 'ryba';
say "ok 1  - sort {cmp} works: [ @a => @y ]";

@a = qw/ryba lufa koza/;
@y = sort { $a cmp $b } @a;
print 'not ' unless $a[0] eq 'ryba' and $a[1] eq 'lufa' and $a[2] eq 'koza';
say "ok 2  - sort {cmp} preserves the original array: [ @a => @y ]";

{
    package Just::For::Fun;

    $a = "something";

    @a = qw/ryba lufa koza/;
    @y = sort { $a cmp $b } @a;
    print 'not ' unless $a eq 'something' and $y[0] eq 'koza' and $y[1] eq 'lufa' and $y[2] eq 'ryba';
    say "ok 3  - sort localizes \$a & \$b properly [ @a => @y ]";
}


use strict;
use feature 'say';

sub x { return 4,5 } 

sub k { my @x = (8, 9); @x } 

sub n { my @x = 8, 9;   @x } 

sub o { my @x = (8, 9); my @y = (11,12); @x, @y } 


print "1..5\n";

my $x   = x(); 
my ($y) = x(); 
my @x   = x(); 
my $expect = "5 / 4 / [4 5]";
my $got    = "$x / $y / [@x]";
print "not " if $expect ne $got;
print "ok 1 - $expect : $got\n"; 

$x = 6, 7; 
@x = 6, 7; 
$expect = "6 [6]";
$got    = "$x [@x]";
print "not " if $expect ne $got;
print "ok 2 - $expect : $got\n"; 


$x = k(); 
@x = k(); 
$expect = "2 [8 9]";
$got    = "$x [@x]";
print "not " if $expect ne $got;
print "ok 3 - $expect : $got\n"; 

$x = n(); 
@x = n(); 
$expect = "1 [8]";
$got    = "$x [@x]";
print "not " if $expect ne $got;
print "ok 4 - $expect : $got\n"; 

$x = o(); 
@x = o(); 
$expect = "2 [8 9 11 12]";
$got    = "$x [@x]";
print "not " if $expect ne $got;
print "ok 5 - $expect : $got\n"; 


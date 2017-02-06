
print "1..1\n";

my $v = x();
print "not " if $v != 123;
print "ok 1  # $_ \n";

# define the subroutine after the line it is called

sub x {
    123
}


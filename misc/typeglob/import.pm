package import;
no strict "refs";

sub import {
    print "importing\n";
    # our $name = "X::v";
    # *$name = \$$name;
    # *X::v = \$X::v;
    my $x;
    *X::v = \$x;
}

sub export {
    print "exporting\n";
    # our $name = "X::v";
    # *$name = \$$name;
    # *X::v = \$X::v;
    my $x;
    *X::v = \$x;
}

1;

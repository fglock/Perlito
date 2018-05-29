package Sub::Name;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::subname" } = \&subname;
    return;
}

sub subname {
    my ($name, $sub) = @_;
    # TODO
    return $sub;
}

1;


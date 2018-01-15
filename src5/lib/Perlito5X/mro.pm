package mro;

sub get_linear_isa {
    my $package = $_[0];
    return _get_linear_isa( $package, [], {} );
}

sub _get_linear_isa {
    no strict 'refs';
    my ($pkg, $packages, $seen) = @_;
    push @$packages, grep !$seen->{$_}++, $pkg;
    for my $package (@{"$pkg\::ISA"}) {
        _get_linear_isa( $package, $packages, $seen ) if !$seen->{$package};
    }
    return $packages;
}

1;

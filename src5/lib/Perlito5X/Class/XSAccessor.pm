
package Class::XSAccessor;

sub import {
    my $class = shift;
    my %args;
    if ( ref($_[0]) eq "HASH" ) {
        %args = %{$_[0]};
    }
    else {
        %args = @_;
    }

    my $caller = caller(0);

    for my $param ( "getters", "setters", "accessors" ) {
        if (ref($args{$param}) eq 'ARRAY') {
            $args{$param} = { map { $_ => $_ } @{$args{$param}} };
        }
    }

    my $getters = delete $args{getters};
    if ($getters) {
        for my $name (keys %$getters) {
            my $key = $getters->{$name};
            *{"${caller}::${name}"} = sub {
                $_[0]->{$key}
            };
        }
    }

    my $setters = delete $args{setters};
    if ($setters) {
        for my $name (keys %$setters) {
            my $key = $setters->{$name};
            *{"${caller}::${name}"} = sub {
                $_[0]->{$key} = $_[1]
            };
        }
    }

    my $accessors = delete $args{accessors};
    if ($accessors) {
        for my $name (keys %$accessors) {
            my $key = $accessors->{$name};
            # print STDERR "generate accessor for $caller $class $name => $key\n";
            #
            # TODO - add message:
            #   "Class::XSAccessor: invalid instance method invocant: no hash ref supplied"
            #
            *{"${caller}::${name}"} = sub {
                @_ > 1
                    ? $_[0]->{$key} = $_[1]
                    : $_[0]->{$key}
            };
        }
    }

    if (keys %args) {
        die "not implemented: Class::XSAccessor @{[ keys %args ]}";
    }
}

1;

__END__

Pure-Perl "Class::XSAccessor" replacement for platforms that don't support XS




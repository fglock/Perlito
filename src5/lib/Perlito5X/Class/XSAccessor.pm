
package Class::XSAccessor;

sub import {
    my $class = shift;
    my %args = @_;

    my $caller = caller(0);
    my $accessors = delete $args{accessors};
    if ($accessors) {
        if (ref($accessors) eq 'ARRAY') {
            $accessors = { map { $_ => $_ } @$accessors };
        }
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




package Perlito5::Clone;

sub clone {
    my @out;
    for my $i ( 0 .. $#_ ) {
        push @out, _clone( $_[$i] );
    }
    return @out;
}

sub _clone {
    my ($obj) = @_;

    return undef if !defined $obj;

    my $ref = ref($obj);
    if ( !$ref ) {
        return $ref;
    }

    if ( $ref eq 'ARRAY' ) {
        my @out;
        for my $i ( 0 .. $#$obj ) {
            push @out, _clone( $obj->[$i] );
        }
        return \@out;
    }
    elsif ( $ref eq 'HASH' ) {
        my @out;
        for my $i ( keys %$obj ) {
            push @out, $i, _clone( $obj->{$i} );
        }
        return {@out};
    }
    elsif ( $ref eq 'SCALAR' || $ref eq 'REF' ) {
        return \( _clone($$obj) );
    }
    elsif ( $ref eq 'CODE' ) {
        return $ref;
    }
    elsif ( $ref eq 'GLOB' ) {
        return $ref;
    }

    my @out;
    my $res;
    local $@;
    $res = eval {
        for my $i ( 0 .. $#$obj ) {
            push @out, _clone( $obj->[$i] ),;
        }
        bless( \@out, $ref );
    };
    return $res if $res;

    # assume it's a blessed HASH
    for my $i ( keys %$obj ) {
        push @out, $i, _clone( $obj->{$i} );
    }
    return bless( {@out}, $ref );
}

1;


package Perlito5::Compare;

use strict;

sub compare {
    _compare( $_[0], $_[1] );
}

sub _compare {
    my ($obj1, $obj2) = @_;

    return 1 if !defined $obj1 && !defined $obj2;
    return 0 if !defined $obj1 || !defined $obj2;

    my $ref1 = ref($obj1);
    my $ref2 = ref($obj2);

    return ($obj1 eq $obj2 ? 1 : 0) if !$ref1 && !$ref2;

    return 0 if $ref1 ne $ref2;

    if ( $ref1 eq 'ARRAY' ) {
        return 0 if $#$obj1 != $#$obj2;
        for my $i ( 0 .. $#$obj1 ) {
            return 0 if ! _compare( $obj1->[$i], $obj2->[$i] );
        }
        return 1;
    }
    elsif ( $ref1 eq 'HASH' ) {
        my @key1 = keys(%$obj1);
        my @key2 = keys(%$obj2);
        return 0 if $#key1 != $#key2;
        for my $i ( @key1 ) {
            return 0 if ! exists $obj2->{$i};
            return 0 if ! compare( $obj1->{$i}, $obj2->{$i} );
        }
        return 1;
    }
    elsif ( $ref1 eq 'SCALAR' || $ref1 eq 'REF' ) {
        return _compare($$obj1, $$obj2);
    }
    elsif ( $ref1 eq 'CODE' ) {
        return 0;   # TODO
    }
    elsif ( $ref1 eq 'GLOB' ) {
        return 0;   # TODO
    }

    # blessed object
    my $res;
    local $@;
    $res = eval { _compare( {%$obj1}, {%$obj2} ) };
    return $res if defined $res;

    $res = eval { _compare( [@$obj1], [@$obj2] ) };
    return $res if defined $res;

    $res = eval { _compare( $$obj1, $$obj2 ) };
    return $res if defined $res;

    return 0; 
}

1;


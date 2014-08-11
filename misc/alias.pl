# implementing alias in perl-to-js
# pseudocode:

sub abc {
    $_[0] = 3;
    return [ $_[0], # returned value
             \@_    # alias values
           ];
}

my $tmp = abc($v);
$v = $tmp->[1][0];  # "alias" $v to $_[0]
$x = $tmp->[0];     # use the returned value

# alternately,
#   variables always have SCALAR/ARRAY/HASH properties
#   @_ is special; it is always an array of aliases
# pseudocode:

var x = { SCALAR: null, ARRAY: [], HASH: {} }

function (List__, p5want) {
            List__.p5aset(0, 3);
            return (List__.p5aget(0));
        };

tmp.SCALAR = abc(
        [ x,             # pass by reference
          { SCALAR: 3 }, # put value in a container
        ], 
        0
);


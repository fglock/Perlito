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


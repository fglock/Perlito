my $s = shift;
if ($s eq '-e') {
    $s = shift;
    eval $s;
}

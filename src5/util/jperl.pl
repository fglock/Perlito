my $s = shift;
if ($s eq '-e') {
    $s = shift;
    print eval $s, "\n";
}

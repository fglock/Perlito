package Perlito5::Runtime;

sub _replace {
    my $s = shift;
    my $old = shift;
    my $new = shift;
    my $p = index($s, $old);
    $p >= 0
    ? substr( $s, 0, $p ) . $new . _replace( substr( $s, $p + length($old) ), $old, $new )
    : $s
}

sub lisp_escape_string {
    my $s = shift;
    _replace($s, "\\", "\\\\");
}

sub to_go_namespace {
    my $s = shift;
    _replace($s, "::", "__");
}

1;


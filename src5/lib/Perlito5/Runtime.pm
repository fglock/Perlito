class Main {

    sub _replace {
        my $s = shift;
        my $old = shift;
        my $new = shift;
        my $p = index($s, $old);
        $p >= 0
        ? substr( $s, 0, $p ) . $new . _replace( substr( $s, $p + $old->chars ), $old, $new )
        : $s
    }

    sub to_lisp_identifier {
        return 'sv-' . $_[0];
    }

    sub lisp_escape_string {
        my $s = shift;
        _replace($s, "\\", "\\\\");
    }

    sub to_javascript_namespace {
        my $s = shift;
        _replace($s, "::", '$');
    }

    sub to_lisp_namespace {
        my $s = shift;
        _replace($s, "::", "-");
    }

    sub to_go_namespace {
        my $s = shift;
        _replace($s, "::", "__");
    }

}


class Main {

    sub _replace($s, $old, $new) {
        my $p = index($s, $old);
        $p >= 0
        ?? substr( $s, 0, $p ) ~ $new ~ _replace( substr( $s, $p + length($old) ), $old, $new )
        !! $s
    }

    sub to_lisp_identifier ( $ident ) {
        return 'sv-' ~ $ident;
    }

    sub lisp_escape_string($s) {
        _replace($s, "\\", "\\\\");
    }

    sub to_javascript_namespace($s) {
        _replace($s, "::", '$');
    }

    sub to_lisp_namespace($s) {
        _replace($s, "::", "-");
    }

    sub to_go_namespace($s) {
        _replace($s, "::", "__");
    }

}


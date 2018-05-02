package utf8;

sub import {
    $Perlito5::UTF8 = 1;
}

sub unimport {
    $Perlito5::UTF8 = 0;
}

sub native_to_unicode {
	return $_[0];
}

sub unicode_to_native {
	return $_[0];
}

sub encode {
	return $_[0];
}

sub upgrade {
	return $_[0];
}

sub downgrade {
	return $_[0];
}

sub is_utf8 {
    # TODO
    if ((0 + $_[0]) eq $_[0]) {
        # looks like number, not utf8
        return 0;
    }
    return 1;
}

1;


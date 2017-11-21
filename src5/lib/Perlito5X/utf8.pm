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

1;


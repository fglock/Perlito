package Perlito5::Runtime::Formline;
use strict;
use warnings;

# a pure Perl implementation of CORE::formline()

sub formline {
    my ($picture, @list) = @_;

    # TODO
    warn "TODO - CORE::formline not implemented";

    my $out = $picture;

    for my $v (@list) {
        $out =~ s{(
                \@   <+           #  @<<<<
              | \@  \|+           #  @||||
              | \@   >+           #  @>>>>
              | \@  \#+  \. \#+   #  @###.###
              | \@  \#+           #  @###
              | \@       \. \#+   #  @.###
              | \@  \*            #  @*
              | \@

              | \^   <+           #  ^<<<<
              | \^  \|+           #  ^||||
              | \^   >+           #  ^>>>>
              | \^  \*            #  ^*
              )
             }
             { _format($1, $v) }xe;
    }

    print "[[ $out ]]\n";

    $^A = $out;
    return 1;
}

sub _format {
    my ($picture, $value) = @_;
    print "_format [[ $picture ]] [[ $value ]]\n";

    while (length($value) < length($picture)) {
        $value = $value . ' ' if $picture =~ / \@\< | \@\| /x;
        $value = ' ' . $value if $picture =~ / \@\> | \@\| /x;
    }
    return substr($value, 0, length($picture));

}

Perlito5::Runtime::Formline::formline(
    "xx @<<<<< xx @||||| xx @>>>>> xx",
        "abc",    "def",    "ghi",
);

1;

__END__

    formline PICTURE,LIST
            This is an internal function used by "format"s, though you may
            call it, too. It formats (see perlform) a list of values according
            to the contents of PICTURE, placing the output into the format
            output accumulator, $^A (or $ACCUMULATOR in English). Eventually,
            when a "write" is done, the contents of $^A are written to some
            filehandle. You could also read $^A and then set $^A back to "".
            Note that a format typically does one "formline" per line of form,
            but the "formline" function itself doesn't care how many newlines
            are embedded in the PICTURE. This means that the "~" and "~~"
            tokens treat the entire PICTURE as a single line. You may
            therefore need to use multiple formlines to implement a single
            record format, just like the "format" compiler.

            Be careful if you put double quotes around the picture, because an
            "@" character may be taken to mean the beginning of an array name.
            "formline" always returns true. See perlform for other examples.

            If you are trying to use this instead of "write" to capture the
            output, you may find it easier to open a filehandle to a scalar
            ("open my $fh, ">", \$output") and write to that instead.


package Perlito5::Runtime::Formline;
use strict;
use warnings;
use Data::Dumper;

# a pure Perl implementation of CORE::formline()

sub formline {
    my ($picture, @list) = @_;

    # TODO
    # TODO - process '~', '~~'
    warn "TODO - CORE::formline not implemented";

    my @parts = split 
             /(
                \@   <+          (?: \.\.\. )?       #  @<<<<     @<<<<... 
              | \@  \|+          (?: \.\.\. )?       #  @||||     @||||...
              | \@   >+          (?: \.\.\. )?       #  @>>>>     @>>>>...
              | \@   0?   \#+  \. \#+                #  @###.###  @0###.###
              | \@   0?   \#+                        #  @###      @0###
              | \@   0?        \. \#+                #  @.###     @0.###
              | \@   0                               #            @0
              | \@  \*                               #  @*
              | \@

              | \^   <+          (?: \.\.\. )?       #  ^<<<<     ^<<<<...  
              | \^  \|+          (?: \.\.\. )?       #  ^||||     ^||||...
              | \^   >+          (?: \.\.\. )?       #  ^>>>>     ^>>>>...
              | \^  \*                               #  ^*
              )/x, $picture;
    print Dumper \@parts;
    my $out = "";
    for my $s (@parts) {
        if ( substr($s, 0, 1) eq "^" ) {
            # special field
            warn "TODO - special field not implemented";
        }
        elsif ( substr($s, 0, 1) eq "@" ) {
            # regular field
            $out .= _format($s, shift @list);
        }
        else {
            $out .= $s;
        }
    }

    print "[[ $out ]]\n";

    $^A = $out;
    return 1;
}

sub _format {
    my ($picture, $value) = @_;
    print "_format [[ $picture ]] [[ $value ]]\n";

    if ($picture eq '@*') {
        chomp($value);
        return $value;
    }

    if ($picture =~ /\@[\.#0]/) {
        my $fmt = "%";
        $fmt .= "0" if $picture =~ /0/;
        $fmt .= length($picture);
        my $dot = index( $picture, "." );
        if ($dot > 0) {
            $fmt = $fmt . "." . ( length($picture) - $dot );
        }
        else {
            $fmt = $fmt . ".0";
        }
        $fmt .= "f";
        print "_format sprintf [[ $fmt ]]\n";
        my $out = sprintf( $fmt, $value );
        print "_format out [[ $out ]]\n";
        return $out;
    }

    if (length($value) < length($picture)) {
        while (length($value) < length($picture)) {
            $value = $value . ' ' if $picture =~ / \@\< | \@\| /x;
            $value = ' ' . $value if $picture =~ / \@\> | \@\| /x;
        }
        return $value;
    }

    if (length($value) > length($picture)) {
        $value = substr($value, 0, length($picture) - 3) . "..." if $picture =~ / \.\.\. /x;
        $value = substr($value, 0, length($picture));
        return $value;
    }

    return $value;
}

Perlito5::Runtime::Formline::formline(
    'xx @<<<<< xx @||||| xx @>>>>> xx @> xx ',
        "abc",    "def",    "ghi",    "zzzz",
);
Perlito5::Runtime::Formline::formline(
    'xx @### xx @###.### xx @.### xx @0####.## xx ',
        13.45,  78.99,      0.12,    14.45,
);
print "done\n";

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


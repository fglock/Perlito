package Perlito5::Runtime::Formline;
use strict;
use warnings;
use Data::Dumper;

# a pure Perl implementation of CORE::formline()
#
# TODO - special variables:  $: $^ $~ $% $= $- $^L
#

sub formline {
    my $picture = shift;

    my $supress_line_if_all_fields_empty = 0;
    my $repeat_line_until_all_fields_exhausted = 0;

    if ($picture =~ /~/) {
        $supress_line_if_all_fields_empty = 1;
        if ($picture =~ /~~/) {
            $repeat_line_until_all_fields_exhausted = 1;
        }
        $picture =~ s/~/ /g;
    }
    $picture =~ s/[ ]*$//;  # trim spaces at the end of line before interpolating
    my @parts = split 
        /( [\@\^]
              (?:
                  <+          (?: \.\.\. )?       #  @<<<<     @<<<<... 
              |  \|+          (?: \.\.\. )?       #  @||||     @||||...
              |   >+          (?: \.\.\. )?       #  @>>>>     @>>>>...
              |   0?   \#+  \. \#+                #  @###.###  @0###.###
              |   0?   \#+                        #  @###      @0###
              |   0?        \. \#+                #  @.###     @0.###
              |   0                               #            @0
              |  \*                               #  @*
              |                                   #  @
              )
        )/x, $picture;
    # print Dumper \@parts;
    do {
        if ($supress_line_if_all_fields_empty) {
            my $empty = 1;
            for (@_) {
                $empty = 0 if defined($_) && $_ ne '';
            }
            $^A = "" if !defined($^A);
            return 1 if $empty;
        }
        my $out = "";
        my $var_index = 0;  # we access the parameter list from @_, because the form parameters are "rw"
      PART:
        for my $part_index (0 .. $#parts) {
            my $s = $parts[$part_index];
            if ( substr($s, 0, 1) eq "^" ) {
                # special field
                my $regular_field = $s;
                $regular_field =~ s/\^/\@/;
                if ($regular_field eq '@*') {
                    $_[$var_index] =~ s/^([^\n*]\n?)//;     # modify the parameter
                    my $var = $1;
                    $var_index++;
                    $out .= _format($regular_field, $var);
                }
                elsif ($regular_field =~ /\@[\.#0]/) {
                    my $var = $_[$var_index++];
                    if (defined($var)) {
                        $out .= _format($regular_field, $var);
                    }
                    else {
                        $out .= " " x length($regular_field);
                    }
                }
                else {
                    if (!defined($_[$var_index]) && $part_index == $#parts) {
                        next PART;                          # skip last field if the variable is undef
                    }
                    my $len = length($regular_field);
                    $_[$var_index] =~ s/^(.{0,$len})//;     # modify the parameter
                    my $var = $1;
                    $var_index++;
                    $out .= _format($regular_field, $var);
                }
            }
            elsif ( substr($s, 0, 1) eq "@" ) {
                # regular field
                $out .= _format($s, $_[$var_index++]);
            }
            else {
                $out .= $s;
            }
        }
        # print "[[ $out ]]\n";
        $^A .= $out;
    }
    while $repeat_line_until_all_fields_exhausted;
    return 1;
}

sub _format {
    my ($picture, $value) = @_;
    # print "_format [[ $picture ]] [[ $value ]]\n";

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
            $fmt = $fmt . "." . ( length($picture) - $dot - 1 );
        }
        else {
            $fmt = $fmt . ".0";
        }
        $fmt .= "f";
        # print "_format sprintf [[ $fmt ]]\n";
        my $out = sprintf( $fmt, $value );
        # print "_format out [[ $out ]]\n";
        if (length($out) > length($picture)) {
            return "#" x length($picture);
        }
        return $out;
    }

    if (length($value) < length($picture)) {
        while (length($value) < length($picture)) {
            $value = $value . ' ' if $picture =~ / \@\< | \@\| /x || $picture eq '@';
            $value = ' ' . $value if $picture =~ / \@\> | \@\| /x && length($value) < length($picture);
        }
        # print "_format smaller [[$value]]\n";
        return $value;
    }

    if (length($value) > length($picture)) {
        $value = substr($value, 0, length($picture) - 3) . "..." if $picture =~ / \.\.\. /x;
        $value = substr($value, 0, length($picture));
        return $value;
    }

    return $value;
}

1;

__END__

# tests

{
    $^A = "";
    Perlito5::Runtime::Formline::formline(
        'xx @<<<<< xx @||||| xx @>>>>> xx @> xx @ xx',
            "abc",    "def",    "ghi",   "jjjj", "k", 
    );
    print "PRF::fl:  [[ $^A ]]\n";
    
    $^A = "";
    Perlito5::Runtime::Formline::formline(
        'xx @### xx @###.### xx @.### xx @0####.## xx @## xx ',
            13.45,  78.99,      0.12,    14.45,       1000,
    );
    print "PRF::fl:  [[ $^A ]]\n";
    
    $^A = "";
    my $v = "abcdefghijklmnop";
    Perlito5::Runtime::Formline::formline(
        'xx ^### xx ^###.### xx ^<<<< xx ^<<<<<<<< xx ^ ^ ^  ',
            13.45,  undef,      $v,      $v,         $v,$v,$v,
    );
    print "PRF::fl:  [[ $^A ]]\n";

    $^A = "";
    $v = "123"; 
    Perlito5::Runtime::Formline::formline( "aaa ^< ^ ^ xxx", $v, $v, $v );
    print "PRF::fl:  ", Dumper [ $^A, $v, defined($v) ];

    $^A = "";
    $v = "";
    formline( "~ xxx ^< xxx", $v );
    print "PRF::fl: ~  ", Dumper [ $^A, $v, defined($v) ];

    $^A = "";
    $v = "1234567";
    formline( "~~ xxx ^< xxx", $v );
    print "PRF::fl: ~~ ", Dumper [ $^A, $v, defined($v) ];

}

{
    $^A = "";
    CORE::formline(
        'xx @<<<<< xx @||||| xx @>>>>> xx @> xx @ xx',
            "abc",    "def",    "ghi",   "jjjj", "k", 
    );
    print "formline: [[ $^A ]]\n";
    
    $^A = "";
    CORE::formline(
        'xx @### xx @###.### xx @.### xx @0####.## xx @## xx ',
            13.45,  78.99,      0.12,    14.45,       1000,
    );
    print "formline: [[ $^A ]]\n";
    
    $^A = "";
    my $v = "abcdefghijklmnop";
    CORE::formline(
        'xx ^### xx ^###.### xx ^<<<< xx ^<<<<<<<< xx ^ ^ ^  ',
            13.45,  undef,      $v,      $v,         $v,$v,$v,
    );
    print "formline: [[ $^A ]]\n";
}

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


package Perlito5::Regex;
use strict;

sub expand_character_range {
    # tr/// style spec

    my @range = split //, shift;
    my @out;

    while (@range) {

        if ( @range >= 2 && $range[0] eq '\\' ) {

            # TODO - \x68 \N{U+20} \200 \n

            push @out, $range[1];
            shift @range for 1..2;
            next;
        }

        if ( @range >= 3 && $range[1] eq '-' ) {
            if (  $range[0] ge 'A' && $range[0] le 'Z'
               && $range[1] eq '-'
               && $range[2] ge 'A' && $range[2] le 'Z'
               )
            {
                push @out, ( $range[0] .. $range[2] );
                shift @range for 1..3;
                next;
            }
            if (  $range[0] ge 'a' && $range[0] le 'z'
               && $range[1] eq '-'
               && $range[2] ge 'a' && $range[2] le 'z'
               )
            {
                push @out, ( $range[0] .. $range[2] );
                shift @range for 1..3;
                next;
            }
            if (  $range[0] ge '0' && $range[0] le '9'
               && $range[1] eq '-'
               && $range[2] ge '0' && $range[2] le '9'
               )
            {
                push @out, ( $range[0] .. $range[2] );
                shift @range for 1..3;
                next;
            }
        }

        push @out, shift(@range);
    }
    return join('', @out);
}

1;


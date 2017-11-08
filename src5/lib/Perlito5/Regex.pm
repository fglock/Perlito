package Perlito5::Regex;
use strict;

# auxiliar sub for expand_character_range()
sub extract_character {
    my ($range) = @_;
    if ($range->[0] ne '\\') {
        return shift(@$range);
    }
    my $res = Perlito5::Grammar::String::double_quoted_unescape($range, 0);
       # to => $pos,
       # capture => Perlito5::AST::Buf->new( buf => chr($tmp) ),
    my $to = $res->{to};
    my $char = $res->{capture}{buf};
    shift @$range for 1..$to;
    return $char;
}

sub expand_character_range {
    # tr/// style spec

    my @range = split //, shift;
    my @out;

    while (@range) {
        push @out, extract_character(\@range);
        if ( @range >= 2 && $range[0] eq '-' ) {
            shift @range;
            my $first = pop @out;
            my $last = extract_character(\@range);
            push @out, map { chr($_) } ( ord($first) .. ord($last) );
        }
    }
    return join('', @out);
}

1;


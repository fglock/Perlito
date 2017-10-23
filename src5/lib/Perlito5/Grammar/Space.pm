package Perlito5::Grammar::Space;

use Perlito5::Grammar::Precedence;

my %line_index;
sub count_line {
    my $pos = $_[0];
    return if $pos < $line_index{$Perlito5::FILE_NAME};
    $line_index{$Perlito5::FILE_NAME} = $pos + 1;
    $Perlito5::LINE_NUMBER++;
}

my %space = (
    '#'     => sub {
                    my $m = Perlito5::Grammar::Space::to_eol($_[0], $_[1]);
                    $m->{to};
                },
    chr(9)  => sub { $_[1] },
    chr(10) => sub {
                    my $str = $_[0];
                    my $pos = $_[1];
                    count_line($pos);
                    $pos++ if $str->[$pos] eq chr(13);
                    my $m = Perlito5::Grammar::Space::start_of_line($_[0], $pos);
                    $m->{to};
                },
    chr(12) => sub { $_[1] },
    chr(13) => sub {
                    my $str = $_[0];
                    my $pos = $_[1];
                    if ($str->[$pos] eq chr(10)) {
                        count_line($pos);
                        $pos++;
                    }
                    my $m = Perlito5::Grammar::Space::start_of_line($_[0], $pos);
                    $m->{to};
                },
    chr(32) => sub { $_[1] },
);


sub term_space {
    my $str = $_[0];
    my $pos = $_[1];
    my $p = $pos;
    while ( $p <= @$str && $space{ $str->[$p] }) {
        $p = $space{ $str->[$p] }->($str, $p+1)
    }
    if ( $str->[$p] eq '_' ) {
        my $s = join( "", @{$str}[ $p .. $p + 6 ] );
        return term_end( $str, $p )
            if $s eq '__END__'
            || $s . $str->[$p+7] eq '__DATA__';
    }
    return { str => $str, from => $pos, to => $p, capture => [ 'space',   ' ' ] }
}

sub term_end {
    my $str = $_[0];
    my $p = $_[1];
    # print STDERR "term_end $p\n";

    my $is_data = 0;
    my $s = join( "", @{$str}[ $p .. $p + 6 ] );
    if ( $s eq '__END__' && $Perlito5::PKG_NAME eq 'main' ) {
        $p = $p + 7;
        $is_data = 1;
    }
    elsif ( $s . $str->[$p+7] eq '__DATA__' ) {
        $p = $p + 8;
        $is_data = 1;
    }
    my $m = Perlito5::Grammar::Space::to_eol($str, $p);
    $p = $m->{to};
    if ( $str->[$p] eq chr(10) ) {
        count_line($p);
        $p++;
        $p++ if $str->[$p] eq chr(13);
    }
    elsif ( $str->[$p] eq chr(13) ) {
        $p++;
        if ($str->[$p] eq chr(10)) {
            count_line($p);
            $p++;
        }
    }
    if ($is_data) {

        my $source = join( "", @$str );
        my $len = length($source);
        $source =~ s/^.*\n#--START--\n# line 1//s;
        my $pos = $p - $len + length($source);

        $Perlito5::DATA_SECTION{ $Perlito5::PKG_NAME } = { pos => $pos, data => $source };
        # leave the DATA filehandle open
        my $pkg = $Perlito5::PKG_NAME;
        open *{$pkg . "::DATA"}, '<', \$Perlito5::DATA_SECTION{$pkg}{data};
        seek(*{$pkg . "::DATA"}, $Perlito5::DATA_SECTION{$pkg}{pos}, 0);
    }
    return { str => $str, from => $_[1], to => scalar(@$str), capture => [ 'space',   ' ' ] }
}

Perlito5::Grammar::Precedence::add_term( '#'        => \&term_space );
Perlito5::Grammar::Precedence::add_term( chr(9)     => \&term_space );
Perlito5::Grammar::Precedence::add_term( chr(10)    => \&term_space );
Perlito5::Grammar::Precedence::add_term( chr(12)    => \&term_space );
Perlito5::Grammar::Precedence::add_term( chr(13)    => \&term_space );
Perlito5::Grammar::Precedence::add_term( chr(32)    => \&term_space );
Perlito5::Grammar::Precedence::add_term( '__END__'  => \&term_end );
Perlito5::Grammar::Precedence::add_term( '__DATA__' => \&term_end );


token to_eol {
    [ <!before [ \c10 | \c13 ]> . ]*
};

token pod_pod_begin {
    [ <!before [ \c10 | \c13 ] '=cut' > . ]*

    [   [ \c10 | \c13 ] '=cut' <.to_eol>
    |   ''
    ]
};

token pod_begin {
    [ <!before [ \c10 | \c13 ] '=end' > . ]*

    [   [ \c10 | \c13 ] '=end' <.to_eol>
    |   ''
    ]
};

token start_of_line {
    <.Perlito5::Grammar::String::here_doc>
    [ '='  [
           |  'pod'      <.pod_pod_begin>
           |  'head'     <.pod_pod_begin>
           |  'item'     <.pod_pod_begin>
           |  'over'     <.pod_pod_begin>
           |  'back'     <.pod_pod_begin>
           |  'begin'    <.pod_begin>
           |  'for'      <.pod_begin>  # TODO - fixme: recognize a single paragraph (double-newline)
           |  'encoding' <.to_eol>
           |  'cut'      <.to_eol> 
           ]
    | '#'
        [ ' ' | \t ]*
        'line'
        [ ' ' | \t ]+
        <Perlito5::Grammar::Number::digits>
        [ ' ' | \t ]*

        # TODO: optional filename (specified with or without quotes)

        <.to_eol>
        {
            $Perlito5::LINE_NUMBER = 0 + Perlito5::Match::flat($MATCH->{'Perlito5::Grammar::Number::digits'});

            # TODO: filename
            # $Perlito5::FILE_NAME   = ...;
        }
    | ''
    ]
};

sub ws {
    my $str = $_[0];
    my $pos = $_[1];
    my $p = $pos;
    while ( $p <= @$str && $space{ $str->[$p] }) {
        $p = $space{ $str->[$p] }->($str, $p+1)
    }
    if ( $str->[$p] eq '_' ) {
        my $s = join( "", @{$str}[ $p .. $p + 6 ] );
        return term_end( $str, $p )
            if $s eq '__END__'
            || $s . $str->[$p+7] eq '__DATA__';
    }
    if ($p == $pos) {
        return;
    }
    return { str => $str, from => $pos, to => $p }
}

sub opt_ws {
    my $str = $_[0];
    my $pos = $_[1];
    my $p = $pos;
    # if ($p == 50) { print STDERR "[[ ", join("", @{$str}), "]]\n"; }
    # print STDERR "$pos: $Perlito5::FILE_NAME $Perlito5::LINE_NUMBER\n";
    while ( $p <= @$str && $space{ $str->[$p] }) {
        $p = $space{ $str->[$p] }->($str, $p+1)
    }
    if ( $str->[$p] eq '_' ) {
        my $s = join( "", @{$str}[ $p .. $p + 6 ] );
        return term_end( $_[0], $p )
            if $s eq '__END__'
            || $s . $str->[$p+7] eq '__DATA__';
    }
    return { str => $_[0], from => $pos, to => $p }
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Space - Grammar for Perlito5 "whitespace"

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

=head1 COPYRIGHT

Copyright 2012 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


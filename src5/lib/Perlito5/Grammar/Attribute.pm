use v5;

package Perlito5::Grammar::Attribute;

sub opt_attribute {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];

    my @attributes;
    my $ws = Perlito5::Grammar::Space->opt_ws( $str, $pos );
    if (substr($str, $ws->{to}, 1) ne ':') {
        # no colon, return an empty list
        return { to => $pos, capture => [] }
    }
    $ws = Perlito5::Grammar::Space->opt_ws( $str, $ws->{to} + 1 );
    my $p = $ws->{to};
    my $m = Perlito5::Grammar->ident($str, $p);
    die "syntax error" if !$m;

    my $to;
    while (1) {

        $to = $m->{to};
        my $delimiter = substr( $str, $to, 1 );
        if ($delimiter eq '(') {
            # "ident(params)"
            my $params = Perlito5::Grammar::String->string_interpolation_parse($str, $m->{to} + 1, '(', ')', 0);
            die "syntax error" if !$params;
            $to = $params->{to};
        }
        push @attributes, substr( $str, $p, $to - $p );

        # check if the attribute list continues
        $ws = Perlito5::Grammar::Space->opt_ws( $str, $to );
        if (substr($str, $ws->{to}, 1) eq ':') {
            $ws = Perlito5::Grammar::Space->opt_ws( $str, $ws->{to} + 1 );
        }
        $p = $ws->{to};
        $m = Perlito5::Grammar->ident($str, $p);

        return { to => $to, capture => \@attributes }
            if ! $m;
    }
}

1;


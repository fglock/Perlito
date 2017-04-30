use v5;

package Perlito5::Grammar::Attribute;

sub opt_attribute {
    my $str = $_[0];
    my $pos = $_[1];

    my @attributes;
    my $ws = Perlito5::Grammar::Space::opt_ws( $str, $pos );
    if ($str->[ $ws->{to} ] ne ':') {
        # no colon, return an empty list
        return { to => $pos, capture => [] }
    }
    $ws = Perlito5::Grammar::Space::opt_ws( $str, $ws->{to} + 1 );
    my $p = $ws->{to};
    my $m = Perlito5::Grammar::ident($str, $p);
    Perlito5::Compiler::error "syntax error" if !$m;

    my $to;
    while (1) {

        my $attr = [ Perlito5::Match::flat($m), undef ];
        $to = $m->{to};
        my $delimiter = $str->[$to];
        if ($delimiter eq '(') {
            # "ident(params)"
            my $params = Perlito5::Grammar::String::string_interpolation_parse($str, $m->{to} + 1, '(', ')', 0);
            Perlito5::Compiler::error "syntax error" if !$params;
            $attr->[1] = Perlito5::Match::flat($params)->{buf};
            $to = $params->{to};
        }
        push @attributes, $attr;

        # check if the attribute list continues
        $ws = Perlito5::Grammar::Space::opt_ws( $str, $to );
        if ($str->[$ws->{to}] eq ':') {
            $ws = Perlito5::Grammar::Space::opt_ws( $str, $ws->{to} + 1 );
        }
        $p = $ws->{to};
        $m = Perlito5::Grammar::ident($str, $p);

        return { to => $to, capture => \@attributes }
            if ! $m;
    }
}

1;


package Perlito5::Dumper;

sub Dumper {
    my $obj = $_[0];
    my $level = $_[1] || 0;

    return 'undef'
        if !defined $obj;

    my $ref = ref($obj);
    my $tab = '    ' x $level;
    my $tab1 = $tab . '    ';

    if ($ref eq 'ARRAY') {
        return "[\n" 
            . join( "", 
                    map($tab1 . Dumper($_, $level+1) . ",\n", @$obj)
                  ) 
            . $tab . ']';
    }
    elsif ($ref eq 'HASH') {
        return "{\n"
            . join( "", 
                    map($tab1 . "'$_' => " . Dumper($obj->{$_}, $level+1) . ",\n", sort keys %$obj)
                  )
            . $tab . '}';
    }
    elsif ($ref) {
        # TODO find out what kind of reference this is (ARRAY, HASH, ...)
        # local $@;
        # eval {
        #     my @data = @$obj;
        #     say "is array";
        #     return 'bless(' . "..." . ", '$ref')";
        # }
        # or eval {
        #     $@ = '';
        #     my %data = %$obj;
        #     say "is hash";
        #     return 'bless(' . "..." . ", '$ref')";
        # };
        # $@ = '';

        # assume it's a blessed HASH

        return "bless({\n"
            . join( "", 
                    map($tab1 . "'$_' => " . Dumper($obj->{$_}, $level+1) . ",\n", sort keys %$obj)
                  )
            . $tab . "}, '$ref')";
    }

    return escape_string($obj);

}

my %safe_char = (
    ' ' => 1,
    '!' => 1,
    '"' => 1,
    '#' => 1,
    '$' => 1,
    '%' => 1,
    '&' => 1,
    '(' => 1,
    ')' => 1,
    '*' => 1,
    '+' => 1,
    ',' => 1,
    '-' => 1,
    '.' => 1,
    '/' => 1,
    ':' => 1,
    ';' => 1,
    '<' => 1,
    '=' => 1,
    '>' => 1,
    '?' => 1,
    '@' => 1,
    '[' => 1,
    ']' => 1,
    '^' => 1,
    '_' => 1,
    '`' => 1,
    '{' => 1,
    '|' => 1,
    '}' => 1,
    '~' => 1,
);

sub escape_string {
    my $s = shift;
    my @out;
    my $tmp = '';
    return "''" if $s eq '';
    for my $i (0 .. length($s) - 1) {
        my $c = substr($s, $i, 1);
        if  (  ($c ge 'a' && $c le 'z')
            || ($c ge 'A' && $c le 'Z')
            || ($c ge '0' && $c le '9')
            || exists( $safe_char{$c} )
            )
        {
            $tmp = $tmp . $c;
        }
        else {
            push @out, "'$tmp'" if $tmp ne '';
            push @out, "chr(" . ord($c) . ")";
            $tmp = '';
        }
    }
    push @out, "'$tmp'" if $tmp ne '';
    return join(' . ', @out);
}


1;


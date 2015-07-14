package Perlito5::Dumper;

sub ast_dumper {
    my $seen  = {};
    my $level = '';
    my $pos   = '[TODO - recursive structure in AST is not supported]';
    return _dumper($_[0], $level, $seen, $pos);
}

# Note: this is called from Perlito5X/Dumper.pm
sub _dumper {
    my ($obj, $tab, $seen, $pos) = @_;

    return 'undef' if !defined $obj;

    my $ref = ref($obj);
    return escape_string($obj) if !$ref;

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;
        
    my $tab1 = $tab . '    ';

    if ($ref eq 'ARRAY') {
        return '[]' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "[\n", @out, $tab, ']');
    }
    elsif ($ref eq 'HASH') {
        return '{}' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "{\n", @out, $tab, '}');
    }
    elsif ($ref eq 'SCALAR') {
        return "\\" . _dumper($$obj, $tab1, $seen, $pos);
    }
    elsif ($ref eq 'CODE') {
        # TODO
        return 'sub { "DUMMY" }';
    }

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
    
    my @out;
    for my $i ( sort keys %$obj ) {
        my $here = $pos . '->{' . $i . '}';
        push @out, 
            $tab1,
            "'$i' => ",
            _dumper($obj->{$i}, $tab1, $seen, $here), 
            ",\n";
    }
    return join('', "bless({\n", @out, $tab, "}, '$ref')");
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
    return 0+$s if (0+$s) eq $s && $s =~ /[0-9]/;
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

sub _identity {
    # returns true if the 2 arguments point to the same reference
    "$_[0]" eq "$_[1]"
}

1;


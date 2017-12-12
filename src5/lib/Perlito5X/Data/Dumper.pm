package Data::Dumper;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::Dumper" } = \&Dumper;
    return;
}

sub Dumper {
    my $seen  = {};
    my $level = '    ';
    my @out;
    for my $i (0 .. $#_) {
        my $pos   = '$VAR' . ($i + 1);
        push @out, "$pos = " . _dumper($_[$i], $level, $seen, $pos) . ";\n";
    }
    return join('', @out);
}

sub _dumper {
    my ($obj, $tab, $seen, $pos) = @_;

    return 'undef' if !defined $obj;

    my $ref = ref($obj);
    if (!$ref) {
        if (ref(\$obj) eq 'GLOB') {
            return "$obj";  # *main::x
        }
        return escape_string($obj);
    }

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
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        return "\\" . _dumper($$obj, $tab1, $seen, $pos);
    }
    elsif ($ref eq 'CODE') {
        # TODO
        return 'sub { "DUMMY" }';
    }
    elsif ($ref eq 'GLOB') {
        return '\\' . *$obj;    # \*main::x
    }

    # local $@;
    
    my @out;
    my $res;
    $res = eval {
        # blessed ARRAY
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        join('', "bless([\n", @out, $tab, "], '$ref')");
    };
    return $res if $res;

    $res = eval {
        # blessed SCALAR
        "bless(\\" . _dumper($$obj, $tab1, $seen, $pos) . ", '$ref')";
    };
    return $res if $res;

    # assume it's a blessed HASH
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

our %safe_char = (
    ' ' => 1,
    '!' => 1,
    '"' => 1,
    "'" => 1,
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
    "\n" => 1,
);

sub escape_string {
    my $s = shift;
    my @out;
    my $tmp = '';
    return "''" if $s eq '';
    return 0+$s if (0+$s) eq $s && $s =~ /[0-9]/;
    for my $c ( split "", $s ) {
        if ( $c eq '\\' || $c eq '$' || $c eq '@' || $c eq '"' ) {
            $tmp = $tmp . '\\' . $c;
        }
        elsif  (  ($c ge 'a' && $c le 'z')
            || ($c ge 'A' && $c le 'Z')
            || ($c ge '0' && $c le '9')
            || exists( $safe_char{$c} )
            )
        {
            $tmp = $tmp . $c;
        }
        else {
            $tmp = $tmp . '\x{' . sprintf("%x", ord($c)) . '}';
        }
    }
    push @out, '"' . $tmp . '"' if $tmp ne '';
    return join(' . ', @out);
}

sub _identity {
    # returns true if the 2 arguments point to the same reference
    "$_[0]" eq "$_[1]"
}

1;


package Data::Dumper;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::Dumper" } = \&Dumper;
    return;
}

sub new {
    my ($class, %args) = @_;
    bless \%args, $class;
}

sub Sortkeys {
    return $_[0];   # TODO
}
sub Useqq {
    return $_[0];   # TODO
}
sub Indent {
    return $_[0];   # TODO
}
sub Terse {
    return $_[0];   # TODO
}
sub Dump {
    return "Data::Dumper Dump() not implemented";   # TODO
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

    local $@;
    
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

    $res = eval {
        # blessed HASH
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        join('', "bless({\n", @out, $tab, "}, '$ref')");
    };
    return $res if $res;

    return join('', "bless(\\" . escape_string("opaque data " . $obj) . ", '$ref')");
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
    map { $_ => 1 } (
            'A' .. 'Z',
            'a' .. 'z',
            '0' .. '9',
        ),
);

sub escape_string {
    my $s = shift;
    return "''" if $s eq '';
    my $v = 0+$s;
    if ($v) {
        return $v if $v eq $s && $s =~ /[0-9]/;
    }
    my @out = '"';
    for my $c ( split "", $s ) {
        if ( $c eq '\\' || $c eq '$' || $c eq '@' || $c eq '"' ) {
            push @out, '\\' . $c;
        }
        elsif ( exists( $safe_char{$c} ) ) {
            push @out, $c;
        }
        else {
            push @out, '\x{' . sprintf("%x", ord($c)) . '}';
        }
    }
    push @out, '"';
    return join('', @out);
}

sub _identity {
    # returns true if the 2 arguments point to the same reference
    "$_[0]" eq "$_[1]"
}

1;


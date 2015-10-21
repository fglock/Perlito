package Perlito5::JSON;

sub ast_dumper {
    my $seen  = {};
    my $level = '';
    my $pos   = '[TODO - recursive structure in AST is not supported]';
    return _dumper($_[0], $level, $seen, $pos);
}

# Note: this is called from Perlito5X/Dumper.pm
sub _dumper {
    my ($obj, $tab, $seen, $pos) = @_;

    return 'null' if !defined $obj;

    my $ref = ref($obj);
    return escape_string($obj) if !$ref;

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;

    my $tab1 = $tab . '  ';

    if ($ref eq 'ARRAY') {
        return '[]' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '[' . $i . ']';
            push @out, 
                $tab1 . _dumper($obj->[$i], $tab1, $seen, $here);
        }
        return "[\n" . join (",\n", @out) . "\n" . $tab . ']';
    }
    elsif ($ref eq 'SCALAR') {
        return '[ "_type": "SCALAR", "value": ' . _dumper($$obj, $tab1, $seen, $pos) . ' ]';
    }
    elsif ($ref eq 'CODE') {
        # TODO
        return '[ "_type": "CODE", "value": { "DUMMY" } ]';
    }

    # assume it's a blessed HASH
    
    $ref =~ s/^Perlito5::AST:://;   # shorten
    my @out;
    push @out, qq{"_type": "$ref"} if $ref ne 'HASH';
    for my $i ( sort keys %$obj ) {
        my $here = $pos . '{' . $i . '}';
        push @out, 
            $tab1 . qq{"$i": } . _dumper($obj->{$i}, $tab1, $seen, $here);
    }
    return "{ " . join (",\n", @out ) . "\n" . $tab . "}";
}

my %safe_char = (
    ' ' => 1,
    '!' => 1,
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
    return qq{""} if $s eq '';
    return 0+$s if (0+$s) eq $s && $s =~ /[0-9]/;
    for my $i (0 .. length($s) - 1) {
        my $c = substr($s, $i, 1);
        if  (  ($c ge 'a' && $c le 'z')
            || ($c ge 'A' && $c le 'Z')
            || ($c ge '0' && $c le '9')
            || exists( $safe_char{$c} )
            )
        {
            push @out, $c;
        }
        elsif ( $c eq "\\" || $c eq '"' || $c eq "/" ) {
            push @out, '\\' . $c;
        }
        elsif ( $c eq "\n" ) {
            push @out, '\\n';
        }
        elsif ( $c eq "\r" ) {
            push @out, '\\r';
        }
        elsif ( $c eq "\t" ) {
            push @out, '\\t';
        }
        elsif ( $c eq "\b" ) {
            push @out, '\\b';
        }
        elsif ( $c eq "\f" ) {
            push @out, '\\f';
        }
        else {
            push @out, $c;     # TODO - encode using unicode
        }
    }
    return join('', '"', @out, '"');
}

sub _identity {
    # returns true if the 2 arguments point to the same reference
    "$_[0]" eq "$_[1]"
}

1;


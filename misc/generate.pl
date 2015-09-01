
sub r { int( sqrt( rand(5) ) ) }

sub gen_ident {
    my @v = qw/ a b x self /;
    return $v[ rand( @v ) ];
}

sub gen_var {
    return '$' . gen_ident();
}

sub gen_bool {
    my $r = rand();
    if ($r > 0.9) {
        return gen_exp() . " && " . gen_exp();
    }
    if ($r > 0.8) {
        return gen_exp() . " || " . gen_exp();
    }
    if ($r > 0.7) {
        return gen_exp() . " // " . gen_exp();
    }
    return gen_var();
}
sub gen_exp {
    my $r = rand();
    if ($r > 0.9) {
        return gen_exp() . " = " . gen_exp();
    }
    if ($r > 0.8) {
        return gen_bool();
    }
    if ($r > 0.7) {
        return gen_bool() . " ? " . gen_exp() . " : " . gen_exp();
    }
    return gen_var();
}

sub gen_stmt {
    my $r = rand();
    if ($r > 0.9) {
        return join "\n", "if (" . gen_bool() . ")" . gen_block();
    }
    if ($r > 0.8) {
        return join "\n", "while (" . gen_bool() . ")" . gen_block();
    }
    if ($r > 0.7) {
        return join "\n", "do " . gen_block(), "while " . gen_bool() . ";";
    }
    return gen_exp . ';';
}

sub gen_block {
    return join "\n", "{",
        ( map { gen_stmt() } 0 .. r() ), "}"
}

print gen_block(), "\n";


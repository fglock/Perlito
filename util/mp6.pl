class Main {
    use MiniPerl6::Go::Emitter;
    use MiniPerl6::Lisp::Emitter;
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Mapping;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;

    my $source;
    my $backend := 'go';
    my $execute := 0;
    if @*ARGS[0] eq '-Bgo' {
        $backend := 'go';
        $execute := 1;
        shift @*ARGS;
    }
    if @*ARGS[0] eq '-Blisp' {
        $backend := 'lisp';
        $execute := 1;
        shift @*ARGS;
    }
    if @*ARGS[0] eq '-Cast-perl6' {
        $backend := 'ast-perl6';
        $execute := 0;
        shift @*ARGS;
    }
    if @*ARGS[0] eq '-e' {
        shift @*ARGS;
        $source := @*ARGS.shift;
    }
    else {
        $source := IO::slurp( @*ARGS.shift );
    }

    warn "// MiniPerl6 compiler";
    warn "// ARGS: ", @*ARGS.perl;
    warn "// backend: ", $backend;

    my $m := MiniPerl6::Grammar.parse( 
        $source, 
        0, 
    );
    my $comp_units := $$m;

    if $backend eq 'ast-perl6' {
        say $comp_units.perl;
    }
    if $backend eq 'go' {
        say CompUnit::emit_go_program( $comp_units );
    }
    if $backend eq 'lisp' {
        for @($comp_units) -> $c {
            say $c.emit_lisp;
        }
    }
}


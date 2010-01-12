class Main {
    use MiniPerl6::Go::Emitter;
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Mapping;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;

    my $source;
    if @*ARGS[0] eq '-e' {
        $source := @*ARGS[1];
    }
    else {
        $source := IO::slurp( @*ARGS[0] );
    }

    say "// MiniPerl6 compiler";
    say "// ARGS: ", @*ARGS.perl;

    my $m := MiniPerl6::Grammar.parse( 
        $source, 
        0, 
    );
    my $comp_units := $$m;
    # say "result: ", $comp_units.perl;
    say CompUnit::emit_go_program( $comp_units );
}


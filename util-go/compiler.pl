class Main {
    use MiniPerl6::Go::Emitter;
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Mapping;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;

    # say "MiniPerl6 compiler";
    my $m := MiniPerl6::Grammar.parse( 
        'class X { has $.abc; 123; say "hello, World!" } class X2 { my $v := 1 }', 
        0, 
    );
    my $comp_units := $$m;
    # say "result: ", $comp_units.perl;
    say CompUnit::emit_go_program( $comp_units );

    # my $s := IO::slurp("lib/Test.pm");
    # say $s;
}


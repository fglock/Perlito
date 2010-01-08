class Main {
    use MiniPerl6::Go::Emitter;
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Mapping;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;
    say "MiniPerl6 compiler";
    # my $m := MiniPerl6::Grammar.comp_unit( 'class X { 123 }', 0 );
    # my $m := MiniPerl6::Grammar.ident( 'abc', 0 );
    my $m := MiniPerl6::Grammar.ident_digit( 'abc', 0 );
    # my $m := MiniPerl6::Grammar.word( 'abc', 0 );

}

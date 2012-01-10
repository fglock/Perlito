class Perlito5 {
    # use Perlito5::Javascript::Runtime;
    # use Perlito5::Javascript::Prelude;
    use Perlito5::Javascript::Emitter;
    use Perlito5::Grammar;
    use Perlito5::Grammar::Control;
    use Perlito5::Grammar::Regex;
    use Perlito5::Emitter::Token;
    use Perlito5::Precedence;
    use Perlito5::Expression;
    use Perlito5::Macro;
    use Perlito5::Runtime;

    sub compile_p5_to_js {
        my $s = shift;
        my $ast = Perlito5::Grammar->exp_stmts($s, 0);
        CompUnit.new( name => 'main', body => $$ast )->emit_javascript()
    }
}


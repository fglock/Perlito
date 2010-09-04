class Perlito {
    # use Perlito::Javascript::Runtime; 
    # use Perlito::Javascript::Prelude; 
    use Perlito::Javascript::Emitter; 
    use Perlito::Grammar;         
    use Perlito::Grammar::Control; 
    use Perlito::Grammar::Regex;   
    use Perlito::Emitter::Token;   
    use Perlito::Precedence;   
    use Perlito::Expression;   

    sub compile_p6_to_js ($s) {
        my $ast = Perlito::Grammar.exp_stmts($s, 0);
        CompUnit.new( name => 'GLOBAL', body => $$ast ).emit_javascript()
    }
}


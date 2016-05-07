class Perlito {
    # use Perlito6::JavaScript::Runtime; 
    # use Perlito6::JavaScript::Prelude; 
    use Perlito6::JavaScript::Emitter; 
    use Perlito6::Grammar;         
    use Perlito6::Grammar::Control; 
    use Perlito6::Grammar::Regex;   
    use Perlito6::Emitter::Token;   
    use Perlito6::Precedence;   
    use Perlito6::Expression;   
    use Perlito6::Macro;   
    use Perlito6::Runtime;

    sub compile_p6_to_js ($s) {
        my $ast = Perlito6::Grammar.exp_stmts($s, 0);
        CompUnit.new( name => 'GLOBAL', body => $$ast ).emit_javascript()
    }
}


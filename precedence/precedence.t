
class Main {
    use Test;
    use MiniPerl6::Expression;
   
    Test::plan 1;
    Test::ok( 
        ($(MiniPerl6::Expression.exp_parse( "123", 0))).perl eq 
        "Val::Int.new('int' => 123)",
        "Int");
    Test::ok( 
        ($(MiniPerl6::Expression.exp_parse( "$abc", 0))).perl eq 
        "Var.new('namespace' => '', 'name' => 'abc', 'twigil' => '', 'sigil' => '$')",
        "Var");
    Test::ok( 
        ($(MiniPerl6::Expression.exp_parse( "2+3", 0))).perl eq 
        "Apply.new('namespace' => '', 'arguments' => [Val::Int.new('int' => 2), Val::Int.new('int' => 3)], 'code' => 'infix:<+>')",
        "infix");
    Test::ok( 
        ($(MiniPerl6::Expression.exp_parse( "2+3*4", 0))).perl eq 
        "Apply.new('namespace' => '', 'arguments' => [Val::Int.new('int' => 2), Apply.new('namespace' => '', 'arguments' => [Val::Int.new('int' => 3), Val::Int.new('int' => 4)], 'code' => 'infix:<*>')], 'code' => 'infix:<+>')",
        "infix precedence");
}

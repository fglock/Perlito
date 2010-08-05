
class Main {
    use Test;
    use MiniPerl6::Expression;
   
    Test::plan 9;
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
    Test::ok( 
        ((MiniPerl6::Expression.exp_parse( "", 0)).capture).perl eq 
        "undef",
        "empty expression");
    Test::ok( 
        ((MiniPerl6::Expression.exp_parse( "[2,3]", 0)).capture).perl eq 
        "Lit::Array.new('array1' => Apply.new('namespace' => '', 'arguments' => [Val::Int.new('int' => 2), Val::Int.new('int' => 3)], 'code' => 'list:<,>'))",
        "array");
    Test::ok( 
        ((MiniPerl6::Expression.exp_parse( "$a[4]", 0)).capture).perl eq 
        "Index.new('obj' => Var.new('namespace' => '', 'name' => 'a', 'twigil' => '', 'sigil' => '$'), 'index_exp' => Val::Int.new('int' => 4))",
        "array index");
    Test::ok( 
        ((MiniPerl6::Expression.exp_parse( "$a[4][5]", 0)).capture).perl eq 
        "Index.new('obj' => Index.new('obj' => Var.new('namespace' => '', 'name' => 'a', 'twigil' => '', 'sigil' => '$'), 'index_exp' => Val::Int.new('int' => 4)), 'index_exp' => Val::Int.new('int' => 5))",
        "array index");
    Test::ok( 
        ((MiniPerl6::Expression.exp_parse( "push $a, 30", 0)).capture).perl eq 
        "Apply.new('namespace' => '', 'arguments' => Apply.new('namespace' => '', 'arguments' => [Var.new('namespace' => '', 'name' => 'a', 'twigil' => '', 'sigil' => '$'), Val::Int.new('int' => 30)], 'code' => 'list:<,>'), 'code' => 'push')",
        "function call");


    # TODO

    # Test::ok( 
    #     ((MiniPerl6::Expression.exp_parse( "push($a, 30)", 0)).capture).perl eq 
    #     "undef",
    #     "function call");
    # Test::ok( 
    #     ((MiniPerl6::Expression.exp_parse( "$a.push(30)", 0)).capture).perl eq 
    #     "undef",
    #     "method call");
    # Test::ok( 
    #     ((MiniPerl6::Expression.exp_parse( "$a.push 30", 0)).capture).perl eq 
    #     "Call.new('hyper' => 0, 'arguments' => Val::Int.new('int' => 30), 'method' => 'push', 'invocant' => Var.new('namespace' => '', 'name' => 'a', 'twigil' => '', 'sigil' => '$'))",
    #     "method call no parenthesis");
    # Test::ok( 
    #     ((MiniPerl6::Expression.exp_parse( "$a.[4]", 0)).capture).perl eq 
    #     "",
    #     "array index method");
    # Test::ok( 
    #     ((MiniPerl6::Expression.exp_parse( "{ a => 2 }", 0)).capture).perl eq 
    #     "undef",
    #     "pair");
}

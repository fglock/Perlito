use v6;

class Main {
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Expression;
    use MiniPerl6::Precedence;
    use MiniPerl6::Emitter::Token;

    use MiniPerl6::Eval;

    say '1..8';

    my $env = 
        [
            {
                'print' =>      sub ( $env, $args ) { 
                                    for @($args) -> $v {
                                        print $v.eval($env);
                                    }
                                    return 1;
                                }, 
                'say' =>        sub ( $env, $args ) { 
                                    for @($args) -> $v {
                                        print $v.eval($env);
                                    }
                                    print "\n";
                                    return 1;
                                }, 
                'infix:<+>' =>  sub ( $env, $args ) { 
                                    ($args[0]).eval($env) + ($args[1]).eval($env)
                                },
                'list:<~>' =>  sub ( $env, $args ) { 
                                    my $s = '';
                                    for @($args) -> $v {
                                        if $v.isa('Val::Buf') {
                                            $v = $v.buf;
                                        }
                                        $s = $s ~ $v;
                                    }
                                    return $s;
                                },
                'infix:<==>' => sub ( $env, $args ) {
                                    ($args[0]).eval($env) == ($args[1]).eval($env)
                                },
                'circumfix:<( )>' => sub ( $env, $args ) {
                                    ($args[0]).eval($env);
                                },
                'infix:<=>' =>  sub ( $env, $args ) {
                                    my $parameters = $args[0];
                                    my $arguments  = $args[1];
                                    if $parameters.isa( 'Lit::Array' ) {
                                        warn "Interpreter TODO: Bind";
                                    }
                                    if $parameters.isa( 'Lit::Hash' ) {
                                        warn "Interpreter TODO: Bind";
                                    }
                                    if $parameters.isa( 'Lit::Object' ) {
                                        warn "Interpreter TODO: Bind";
                                    }
                                    if $parameters.isa( 'Decl' ) {
                                        $parameters.eval($env);
                                    }
                                    my $name  = $parameters.plain_name;
                                    my $value = $arguments.eval($env);
                                    for @($env) -> $e {
                                        if exists( $e{ $name } ) {
                                            $e{ $name } = $value;
                                            return $value;
                                        }
                                    }
                                    warn "Interpreter Bind: variable '" ~ $name ~ "' not found";
                                },
            }
        ];

    my $m = Val::Int.new( int => 123 );
    if ($m.eval) eq 123 {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m = Apply.new(
                code      => 'say',
                namespace => '',
                arguments => [ Val::Buf.new( buf => '# ok eval-ast' ) ],
            );
    $m.eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '
                say "# ok eval-string ", (3+4), "==7";
        ', 
        0
    );
    ($$m).>>eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '
                my $a = 3;
                say "# ok eval-string ", $a;
        ', 
        0
    );
    ($$m).>>eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '
                my $a = 123;
                if $a == (100 + 23) {
                    say "# looks good";
                }
                else {
                    print "not ";
                }
                say "ok 2 # eval-string ", $a;
        ', 
        0
    );
    # say ($$m).perl;
    ($$m).>>eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '
                my @a = [ 3, 4 ];
                my $x = 5;
                for @a -> $x {
                    say "ok ", $x, " # eval-string for";
                }
                say "ok ", $x, " # eval-string outer-lexical";
        ', 
        0
    );
    # say ($$m).perl;
    ($$m).>>eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '

                sub add2 ($v) { $v + 2 } 

                say "ok ", add2(4), " # eval-string named sub";
        ', 
        0
    );
    # say ($$m).perl;
    ($$m).>>eval( $env );

    $m = MiniPerl6::Grammar.exp_stmts( 
        '
                my %v = { a => 5, b => 6, c => 7 }; 
                my @x = [ 5, 6, 7, 8, 9, 10 ];
                say "ok ", %v{"c"}, " # eval-string hash literal and lookup";
                say "ok ", @x[3], " # eval-string array literal and lookup";
        ', 
        0
    );
    # say ($$m).perl;
    ($$m).>>eval( $env );

}

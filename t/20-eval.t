use v6;

class BuiltinFunction {
    has $.func;

    method eval ( $env, $args ) {
        $.func.( $env, $args );
    }
}

class Main {
    use MiniPerl6::Grammar;
    use MiniPerl6::Eval;

    say '1..1';

    my $env := 
        [
            {
                'print' => ::BuiltinFunction(
                        func => sub ( $env, $args ) { 
                                    for @($args) -> $v {
                                        print $v.eval($env);
                                    }
                                    return 1;
                                }, 
                    ),
                'say' => ::BuiltinFunction(
                        func => sub ( $env, $args ) { 
                                    for @($args) -> $v {
                                        print $v.eval($env);
                                    }
                                    print "\n";
                                    return 1;
                                }, 
                    ),
                'infix:<+>' => ::BuiltinFunction(
                        func => sub ( $env, $args ) { 
                                    ($args[0]).eval($env) + ($args[1]).eval($env)
                                },
                    ),
                'infix:<==>' => ::BuiltinFunction(
                        func => sub ( $env, $args ) {
                                    ($args[0]).eval($env) == ($args[1]).eval($env)
                                },
                    ),
            }
        ];

    my $m := ::Val::Num( num => 123 );
    if ($m.eval) eq 123 {
        say 'ok 1';
    }
    else {
        say 'not ok 1';
    }

    $m := ::Apply(
                code      => 'say',
                namespace => '',
                arguments => [ ::Val::Buf( buf => '# ok eval-ast' ) ],
            );
    $m.eval( $env );

    $m := MiniPerl6::Grammar.comp_unit( 
        '
            class Testing { 
                say "# ok eval-string ", (3+4), "==7";
            }
        ', 
        0
    );
    ($$m).eval( $env );

    $m := MiniPerl6::Grammar.comp_unit( 
        '
            class Testing { 
                my $a := 3;
                say "# ok eval-string ", $a;
            }
        ', 
        0
    );
    ($$m).eval( $env );

    $m := MiniPerl6::Grammar.comp_unit( 
        '
            class Testing { 
                my $a := 123;
                if $a == (100 + 23) {
                    say "# looks good";
                }
                else {
                    print "not ";
                }
                say "ok 2 # eval-string ", $a;
            }
        ', 
        0
    );
    # say ($$m).perl;
    ($$m).eval( $env );

}

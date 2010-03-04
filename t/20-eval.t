use v6;

class BuiltinFunction {
    has $.func;

    method eval ( $env, $args ) {
        $.func.( $env, $args );
    }
}

class Main {
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
                arguments => [ ::Val::Buf( buf => '# ok print()' ) ],
            );
    $m.eval( $env );

}

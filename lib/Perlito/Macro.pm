class Lit::Array {
    has @.array1;
    method expand_interpolation {
        my $needs_interpolation = 0;
        my @items;
        for @.array1 -> $item {
            if $item.isa( 'Apply' ) && ( $item.code eq 'circumfix:<( )>' || $item.code eq 'list:<,>' ) {
                for @($item.arguments) -> $arg {
                    @items.push($arg);
                }
            }
            else {
                @items.push($item);
            }
        }
        for @items -> $item {
            if      $item.isa( 'Var' )   && $item.sigil eq '@'
                ||  $item.isa( 'Apply' ) && ( $item.code eq 'prefix:<@>' || $item.code eq 'infix:<..>' )
            {
                $needs_interpolation = 1;
            }
        }
        if $needs_interpolation && @items.elems() == 1 {
            return @items[0];
        }
        if $needs_interpolation {
            my @s;
            for @items -> $item {
                if      $item.isa( 'Var' )   && $item.sigil eq '@'
                    ||  $item.isa( 'Apply' ) && ( $item.code eq 'prefix:<@>' || $item.code eq 'infix:<..>' )
                {
                    push @s,
                        Apply.new(
                            'arguments' => [
                                    Var.new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 
                                    $item
                                ], 
                            'code' => 'infix:<=>', 
                            'namespace' => ''
                        );
                    push @s, 
                        For.new(
                            'body' => Lit::Block.new(
                                'sig' => Var.new('name' => 'x', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 
                                'stmts' => [
                                    Call.new('arguments' => [Index.new('index_exp' => Var.new('name' => 'x', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 'obj' => Var.new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''))], 'hyper' => '', 'invocant' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 'method' => 'push')
                                ]
                            ), 
                            'cond' => Apply.new(
                                'arguments' => [
                                    Val::Int.new('int' => 0), 
                                    Apply.new('arguments' => [Apply.new('arguments' => [Call.new('arguments' => [], 'hyper' => '', 'invocant' => Var.new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 'method' => 'elems'), Val::Int.new('int' => 1)], 'code' => 'infix:<->', 'namespace' => '')], 'code' => 'circumfix:<( )>', 'namespace' => '')
                                ], 
                                'code' => 'infix:<..>', 
                                'namespace' => ''
                            ), 
                            'topic' => Mu
                        ); 
                }
                else {
                    push @s, 
                        Call.new(
                            'arguments' => [ $item ], 
                            'hyper' => '', 
                            'invocant' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 
                            'method' => 'push');
                }
            }
            return Do.new(
                'block' => Lit::Block.new(
                    'sig' => Mu, 
                    'stmts' => [
                        Decl.new(
                            'decl' => 'my', 
                            'type' => '', 
                            'var' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => '')), 
                        Decl.new(
                            'decl' => 'my', 
                            'type' => '', 
                            'var' => Var.new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => '')),
                        @s,
                        Var.new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                    ],
                ),
            );
        }
        else {
            return Lit::Array.new( array1 => @items );
        }
    }
}



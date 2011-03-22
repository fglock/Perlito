use v6;

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
}

class Lit::Hash {
    has @.hash1;
    method expand_interpolation {
        my @items;
        for @.hash1 -> $item {
            if $item.isa( 'Apply' ) && ( $item.code eq 'circumfix:<( )>' || $item.code eq 'list:<,>' ) {
                for @($item.arguments) -> $arg {
                    @items.push($arg);
                }
            }
            else {
                @items.push($item);
            }
        }
        my @s;
        for @items -> $item {
            if $item.isa('Apply') && $item.code eq 'infix:<=>>' {
                push @s,
                    Apply.new(
                        'arguments' => [
                            Lookup.new(
                                'index_exp' => $item.arguments[0],
                                'obj' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')), 
                            $item.arguments[1]
                        ], 
                        'code' => 'infix:<=>', 
                        'namespace' => '');
            }
            elsif   $item.isa( 'Var' )   && $item.sigil eq '%'
                ||  $item.isa( 'Apply' ) && $item.code eq 'prefix:<%>' 
            {
                push @s,
                    For.new(
                        'body' => Lit::Block.new(
                            'sig' => Var.new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 
                            'stmts' => [Apply.new(
                                    'arguments' => [
                                        Lookup.new(
                                            'index_exp' => Call.new(
                                                'arguments' => Mu, 
                                                'hyper' => '', 
                                                'invocant' => Var.new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 
                                                'method' => 'key'), 
                                            'obj' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')), 
                                        Call.new('arguments' => [], 'hyper' => '', 'invocant' => Var.new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 'method' => 'value')
                                    ], 
                                    'code' => 'infix:<=>', 
                                    'namespace' => '')
                                ]
                            ), 
                        'cond' => Apply.new(
                            'arguments' => [
                                Apply.new(
                                    'arguments' => [
                                        Call.new(
                                            'arguments' => Mu, 
                                            'hyper' => '', 
                                            'invocant' => $item, 
                                            'method' => 'pairs')
                                    ], 
                                    'code' => 'circumfix:<( )>', 
                                    'namespace' => '')
                            ], 
                            'code' => 'prefix:<@>', 
                            'namespace' => ''), 
                        'topic' => Mu
                    );
            }
            else {
                die 'Error in hash composer: ', $item.perl;
            }
        }
        return Do.new(
            'block' => Lit::Block.new(
                'sig' => Mu, 
                'stmts' => [
                    Decl.new(
                        'decl' => 'my', 
                        'type' => '', 
                        'var' => Var.new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')), 
                    @s,
                    Var.new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => ''),
                ],
            ),
        );
    }
}

=begin

=head1 NAME

Perlito::Macro - Ast macros for Perlito

=head1 SYNOPSIS

    $ast = $ast.expand_interpolation()

=head1 DESCRIPTION

This module implements some Ast transformations for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2011 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


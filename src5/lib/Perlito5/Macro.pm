use v5;

class Lit::Array {
    sub expand_interpolation {
        my $self = $_[0];

        my $needs_interpolation = 0;
        my @items;
        for my $item ( @.array1 ) {
            if $item->isa( 'Apply' ) && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' ) {
                for my $arg ( @($item->arguments) ) {
                    push( @items, $arg);
                }
            }
            else {
                push( @items, $item);
            }
        }
        for my $item ( @items ) {
            if      $item->isa( 'Var' )   && $item->sigil eq '@'
                ||  $item->isa( 'Apply' ) && ( $item->code eq 'prefix:<@>' || $item->code eq 'infix:<..>' )
            {
                $needs_interpolation = 1;
            }
        }
        if $needs_interpolation && @items->elems() == 1 {
            return @items[0];
        }
        my @s;
        for my $item ( @items ) {
            if      $item->isa( 'Var' )   && $item->sigil eq '@'
                ||  $item->isa( 'Apply' ) && ( $item->code eq 'prefix:<@>' || $item->code eq 'infix:<..>' )
            {
                push @s,
                    Apply->new(
                        'arguments' => [
                                Var->new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                                $item
                            ],
                        'code' => 'infix:<=>',
                        'namespace' => ''
                    );
                push @s,
                    For->new(
                        'body' => Lit::Block->new(
                            'sig' => Var->new('name' => 'x', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                            'stmts' => [
                                Call->new('arguments' => [Index->new('index_exp' => Var->new('name' => 'x', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 'obj' => Var->new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''))], 'hyper' => '', 'invocant' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 'method' => 'push')
                            ]
                        ),
                        'cond' => Apply->new(
                            'arguments' => [
                                Val::Int->new('int' => 0),
                                Apply->new('arguments' => [Apply->new('arguments' => [Call->new('arguments' => [], 'hyper' => '', 'invocant' => Var->new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => ''), 'method' => 'elems'), Val::Int->new('int' => 1)], 'code' => 'infix:<->', 'namespace' => '')], 'code' => 'circumfix:<( )>', 'namespace' => '')
                            ],
                            'code' => 'infix:<..>',
                            'namespace' => ''
                        ),
                        'topic' => Mu
                    );
            }
            else {
                push @s,
                    Call->new(
                        'arguments' => [ $item ],
                        'hyper' => '',
                        'invocant' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                        'method' => 'push');
            }
        }
        return Do->new(
            'block' => Lit::Block->new(
                'sig' => Mu,
                'stmts' => [
                    Decl->new(
                        'decl' => 'my',
                        'type' => '',
                        'var' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => '')),
                    Decl->new(
                        'decl' => 'my',
                        'type' => '',
                        'var' => Var->new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => '')),
                    @s,
                    Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                ],
            ),
        );
    }
}

class Lit::Hash {
    sub expand_interpolation {
        my $self = $_[0];

        my @items;
        for my $item ( @.hash1 ) {
            if $item->isa( 'Apply' ) && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' ) {
                for my $arg ( @($item->arguments) ) {
                    push( @items, $arg);
                }
            }
            else {
                push( @items, $item);
            }
        }
        my @s;
        for my $item ( @items ) {
            if $item->isa('Apply') && $item->code eq 'infix:<=>>' {
                push @s,
                    Apply->new(
                        'arguments' => [
                            Lookup->new(
                                'index_exp' => $item->arguments[0],
                                'obj' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')),
                            $item->arguments[1]
                        ],
                        'code' => 'infix:<=>',
                        'namespace' => '');
            }
            elsif   $item->isa( 'Var' )   && $item->sigil eq '%'
                ||  $item->isa( 'Apply' ) && $item->code eq 'prefix:<%>'
            {
                push @s,
                    For->new(
                        'body' => Lit::Block->new(
                            'sig' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                            'stmts' => [Apply->new(
                                    'arguments' => [
                                        Lookup->new(
                                            'index_exp' => Call->new(
                                                'arguments' => Mu,
                                                'hyper' => '',
                                                'invocant' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                                                'method' => 'key'),
                                            'obj' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')),
                                        Call->new('arguments' => [], 'hyper' => '', 'invocant' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 'method' => 'value')
                                    ],
                                    'code' => 'infix:<=>',
                                    'namespace' => '')
                                ]
                            ),
                        'cond' => Apply->new(
                            'arguments' => [
                                Apply->new(
                                    'arguments' => [
                                        Call->new(
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
            elsif   $item->isa( 'Var' )   && $item->sigil eq '@'
                ||  $item->isa( 'Apply' ) && $item->code eq 'prefix:<@>'
            {

                # do {
                #     my $_i = 0;
                #     my @_a = @{ expr };
                #     while ( $_i < scalar(@_a) ) { $a{ $_a[$_i] } = $a[ $_i + 1 ]; $_i = $_i + 2 }
                #   }

                push @s,
                    Do->new(
                        'block' => Lit::Block->new(
                            'sig'   => Mu,
                            'stmts' => [
                                Apply->new( 'arguments' => [ Decl->new( 'decl' => 'my', 'type' => '', 'var' => Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ) ), Val::Int->new( 'int' => 0 ) ], 'code' => 'infix:<=>', 'namespace' => '' ),
                                Apply->new( 
                                    'arguments' => [ 
                                        Decl->new( 'decl' => 'my', 'type' => '', 'var' => Var->new( 'name' => '_a', 'namespace' => '', 'sigil' => '@', 'twigil' => '' ) ), 
                                        $item
                                    ], 
                                    'code' => 'infix:<=>', 
                                    'namespace' => '' 
                                ),
                                While->new(
                                    'body' => Lit::Block->new(
                                        'sig'   => Mu,
                                        'stmts' => [
                                            Apply->new( 
                                                'arguments' => [ 
                                                    Lookup->new( 
                                                        'index_exp' => Index->new( 'index_exp' => Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), 'obj' => Var->new( 'name' => '_a', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ) ), 
                                                        'obj' => Var->new( 'name' => 'a', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ) ), 
                                                    Index->new( 'index_exp' => Apply->new( 'arguments' => [ Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), Val::Int->new( 'int' => 1 ) ], 'code' => 'infix:<+>', 'namespace' => '' ), 'obj' => Var->new( 'name' => '_a', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ) ) 
                                                ], 
                                                'code' => 'infix:<=>', 
                                                'namespace' => '' 
                                            ),
                                            Apply->new( 'arguments' => [ Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), Apply->new( 'arguments' => [ Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), Val::Int->new( 'int' => 2 ) ], 'code' => 'infix:<+>', 'namespace' => '' ) ], 'code' => 'infix:<=>', 'namespace' => '' )
                                        ]
                                    ),
                                    'cond' => Apply->new( 'arguments' => [ Apply->new( 'arguments' => [ Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), Call->new( 'hyper' => '', 'invocant' => Var->new( 'name' => '_a', 'namespace' => '', 'sigil' => '@', 'twigil' => '' ), 'method' => 'elems' ) ], 'code' => 'infix:<<>', 'namespace' => '' ) ], 'code' => 'circumfix:<( )>', 'namespace' => '' )
                                )
                            ]
                        )
                    );

            }
            else {
                die 'Error in hash composer: ', $item->perl;
            }
        }
        return Do->new(
            'block' => Lit::Block->new(
                'sig' => Mu,
                'stmts' => [
                    Decl->new(
                        'decl' => 'my',
                        'type' => '',
                        'var' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')),
                    @s,
                    Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => ''),
                ],
            ),
        );
    }
}

class Apply {

    my %op = (
        'infix:<+=>'  => 'infix:<+>',
        'infix:<-=>'  => 'infix:<->',
        'infix:<*=>'  => 'infix:<*>',
        'infix:</=>'  => 'infix:</>',
        'infix:<||=>' => 'infix:<||>',
        'infix:<&&=>' => 'infix:<&&>',
        'infix:<|=>'  => 'infix:<|>',
        'infix:<&=>'  => 'infix:<&>',
        'infix:<//=>' => 'infix:<//>',
        'infix:<.=>'  => 'list:<.>',
    );

    sub op_assign {
        my $self = $_[0];

        my $code = $.code;
        return 0 unless $code->isa( 'Str' );

        if exists( %op{$code} ) {
            return Apply->new(
                code      => 'infix:<=>',
                arguments => [
                    @.arguments[0],
                    Apply->new(
                        code      => %op{$code},
                        arguments => @.arguments,
                    ),
                ]
            );
        }

        return 0;
    }
}

class Do {
    sub simplify {
        my $self = $_[0];

        my $block;
        if $.block->isa('Lit::Block') {
            $block = $.block->stmts;
        }
        else {
            $block = [ $.block ]
        }
        if $block->elems() == 1 {
            # optimize some code generated by the regex compiler
            my $stmt = $block->[0];
            if $stmt->isa('Apply') && $stmt->code() eq 'circumfix:<( )>' {
                my $args = $stmt->arguments;
                return Do->new( block => $args->[0] )->simplify;
            }
            if $stmt->isa('Do') {
                return $stmt->simplify;
            }
        }
        return Do->new( block => $block );
    }
}

=begin

=head1 NAME

Perlito5::Macro - Ast macros for Perlito

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


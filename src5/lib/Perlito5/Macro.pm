use v5;

package Lit::Array;
sub expand_interpolation {
    my $self = $_[0];

    my $needs_interpolation = 0;
    my @items;
    for my $item ( @{$self->{"array1"}} ) {
        if ($item->isa( 'Apply' ) && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' )) {
            for my $arg ( @{$item->arguments} ) {
                push( @items, $arg);
            }
        }
        else {
            push( @items, $item);
        }
    }
    for my $item ( @items ) {
        if  (   $item->isa( 'Var' )   && $item->sigil eq '@'
            ||     $item->isa( 'Apply' ) 
                && ( $item->code eq 'prefix:<@>' || $item->code eq 'infix:<..>' || $item->code eq 'map' )
            )
        {
            $needs_interpolation = 1;
        }
    }
    if ($needs_interpolation && scalar(@items) == 1) {
        return
            Apply->new(
                arguments => [
                        $items[0]
                    ],
                code => 'prefix:<\\>',
                namespace => '',
            );
    }
    my @s;
    for my $item ( @items ) {
        if  (   $item->isa( 'Var' )   && $item->sigil eq '@'
            ||     $item->isa( 'Apply' ) 
                && ( $item->code eq 'prefix:<@>' || $item->code eq 'infix:<..>' || $item->code eq 'map' )
            )
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
                            Apply->new(
                                'arguments' => [
                                    Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                                    Index->new('index_exp' => Var->new('name' => 'x', 'namespace' => '', 'sigil' => '$', 'twigil' => ''), 'obj' => Var->new('name' => 'v', 'namespace' => '', 'sigil' => '$', 'twigil' => ''))],
                                'code' => 'push',
                                'namespace' => '')
                        ]
                    ),
                    'cond' => Apply->new(
                        'arguments' => [
                            Val::Int->new('int' => 0),
                            Apply->new(
                                'arguments' => [
                                    Apply->new(
                                        'arguments' => [
                                            Apply->new(
                                                'arguments' => [
                                                    Var->new('name' => 'v', 'namespace' => '', 'sigil' => '@', 'twigil' => '')],
                                                'code' => 'scalar',
                                                'namespace' => ''), 
                                            Val::Int->new('int' => 1)
                                        ],
                                        'code' => 'infix:<->', 'namespace' => '')],
                                'code' => 'circumfix:<( )>',
                                'namespace' => '')
                        ],
                        'code' => 'infix:<..>',
                        'namespace' => ''
                    ),
                    'topic' => undef
                );
        }
        else {
            push @s,
                Apply->new(
                    'arguments' => [
                        Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                        $item ],
                    'code' => 'push',
                    'namespace' => '');
        }
    }
    return Do->new(
        'block' => Lit::Block->new(
            'sig' => undef,
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
                Apply->new(
                    arguments => [
                            Var->new('name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => ''),
                        ],
                    code => 'prefix:<\\>',
                    namespace => '',
                ),
            ],
        ),
    );
}


package Lit::Hash;
sub expand_interpolation {
    my $self = $_[0];

    my @items;
    for my $item ( @{$self->{"hash1"}} ) {
        if ($item->isa( 'Apply' ) && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' )) {
            for my $arg ( @{$item->arguments} ) {
                push( @items, $arg);
            }
        }
        else {
            push( @items, $item);
        }
    }
    my @s;
    for my $item ( @items ) {
        if ($item->isa('Apply') && $item->code eq 'infix:<=>>') {
            push @s,
                Apply->new(
                    'arguments' => [
                        Lookup->new(
                            'index_exp' => $item->arguments[0],
                            'obj' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '$', 'twigil' => '')),
                        $item->arguments[1]
                    ],
                    'code' => 'infix:<=>',
                    'namespace' => '');
        }
        elsif (   $item->isa( 'Var' )   && $item->sigil eq '%'
              ||  $item->isa( 'Apply' ) && $item->code eq 'prefix:<%>'
              )
        {
            my $v = $item;
            $v = Var->new( sigil => '$', twigil => $item->twigil, namespace => $item->namespace, name => $item->name )
                if $item->isa( 'Var' );
            push @s,
                For->new(
                    'body' => Lit::Block->new(
                                'sig' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                                'stmts' => [Apply->new(
                                        'arguments' => [
                                            Lookup->new(
                                                    'index_exp' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                                                    'obj' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '$', 'twigil' => '')
                                                ),
                                            Lookup->new(
                                                    'index_exp' => Var->new('name' => 'p', 'namespace' => '', 'sigil' => '$', 'twigil' => ''),
                                                    'obj' => $v
                                                ),
                                        ],
                                        'code' => 'infix:<=>',
                                        'namespace' => '')
                                    ]
                            ),
                    'cond' => Apply->new(
                                'arguments' => [ $item ],
                                'code'      => 'keys',
                                'namespace' => ''
                            ),
                    'topic' => undef
                );
        }
        elsif (   $item->isa( 'Var' )   && $item->sigil eq '@'
              ||  $item->isa( 'Apply' ) && $item->code eq 'prefix:<@>'
              )
        {

            # do {
            #     my $_i = 0;
            #     my @_a = @{ expr };
            #     while ( $_i < scalar(@_a) ) { $a{ $_a[$_i] } = $a[ $_i + 1 ]; $_i = $_i + 2 }
            #   }

            push @s,
                Do->new(
                    'block' => Lit::Block->new(
                        'sig'   => undef,
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
                                    'sig'   => undef,
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
                                'cond' => Apply->new( 'arguments' => [ 
                                        Apply->new( 'arguments' => [ 
                                            Var->new( 'name' => '_i', 'namespace' => '', 'sigil' => '$', 'twigil' => '' ), 
                                            Apply->new(
                                                'arguments' => [Var->new( 'name' => '_a', 'namespace' => '', 'sigil' => '@', 'twigil' => '' )], 'code' => 'scalar', 'namespace' => '' 
                                            ) ], 
                                        'code' => 'infix:<<>', 'namespace' => '' ) 
                                    ], 'code' => 'circumfix:<( )>', 'namespace' => '' )
                            )
                        ]
                    )
                );

        }
        else {
            die 'Error in hash composer: ', $item;
        }
    }
    return Do->new(
        'block' => Lit::Block->new(
            'sig' => undef,
            'stmts' => [
                Decl->new(
                    'decl' => 'my',
                    'type' => '',
                    'var' => Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => '')),
                @s,
                Apply->new(
                    arguments => [
                            Var->new('name' => 'a', 'namespace' => '', 'sigil' => '%', 'twigil' => ''),
                        ],
                    code => 'prefix:<\\>',
                    namespace => '',
                ),
            ],
        ),
    );
}


package Apply;

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

    my $code = $self->{"code"};
    return 0 if ref($code);

    if (exists( $op{$code} )) {
        return Apply->new(
            code      => 'infix:<=>',
            arguments => [
                $self->{"arguments"}->[0],
                Apply->new(
                    code      => $op{$code},
                    arguments => $self->{"arguments"},
                ),
            ]
        );
    }

    return 0;
}


package Do;
sub simplify {
    my $self = $_[0];

    my $block;
    if ($self->{"block"}->isa('Lit::Block')) {
        $block = $self->{"block"}->stmts;
    }
    else {
        $block = [ $self->{"block"} ]
    }
    if (scalar(@$block) == 1) {
        # optimize some code generated by the regex compiler
        my $stmt = $block->[0];
        if ($stmt->isa('Apply') && $stmt->code() eq 'circumfix:<( )>') {
            my $args = $stmt->arguments;
            return Do->new( block => $args->[0] )->simplify;
        }
        if ($stmt->isa('Do')) {
            return $stmt->simplify;
        }
    }
    return Do->new( block => $block );
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


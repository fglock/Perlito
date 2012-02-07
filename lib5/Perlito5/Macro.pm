# Do not edit this file - Generated by Perlito5 8.0
use v5;
use utf8;
use strict;
use warnings;
no warnings ('redefine', 'once', 'void', 'uninitialized', 'misc', 'recursion');
use Perlito5::Perl5::Runtime;
our $MATCH = Perlito5::Match->new();
package main;
use v5;
package Lit::Array;
sub expand_interpolation {
    ((my  $self) = $_[0]);
    ((my  $needs_interpolation) = 0);
    (my  @items);
    for my $item (@{$self->{('array1')}}) {
        if ((($item->isa('Apply') && ((($item->code() eq 'circumfix:<( )>') || ($item->code() eq 'list:<,>')))))) {
            for my $arg (@{$item->arguments()}) {
                push(@items, $arg )
            }
        }
        else {
            push(@items, $item )
        }
    };
    for my $item (@items) {
        if (((($item->isa('Var') && ($item->sigil() eq chr(64))) || ($item->isa('Apply') && (((($item->code() eq 'prefix:<' . chr(64) . '>') || ($item->code() eq 'infix:<..>')) || ($item->code() eq 'map'))))))) {
            ($needs_interpolation = 1)
        }
    };
    if ((($needs_interpolation && (scalar(@items) == 1)))) {
        return (Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, $items[0] );
    \@a
})), ('code' => 'circumfix:<[ ]>'), ('namespace' => '')))
    };
    (my  @s);
    for my $item (@items) {
        if (((($item->isa('Var') && ($item->sigil() eq chr(64))) || ($item->isa('Apply') && (((($item->code() eq 'prefix:<' . chr(64) . '>') || ($item->code() eq 'infix:<..>')) || ($item->code() eq 'map'))))))) {
            push(@s, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'v'), ('namespace' => ''), ('sigil' => chr(64))) );
    push(@a, $item );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
            push(@s, For->new(('body' => Lit::Block->new(('sig' => Var->new(('name' => 'x'), ('namespace' => ''), ('sigil' => chr(36)))), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(64))) );
    push(@a, Index->new(('index_exp' => Var->new(('name' => 'x'), ('namespace' => ''), ('sigil' => chr(36)))), ('obj' => Var->new(('name' => 'v'), ('namespace' => ''), ('sigil' => chr(36))))) );
    \@a
})), ('code' => 'push'), ('namespace' => '')) );
    \@a
})))), ('cond' => Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Val::Int->new(('int' => 0)) );
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'v'), ('namespace' => ''), ('sigil' => chr(64))) );
    \@a
})), ('code' => 'scalar'), ('namespace' => '')) );
    push(@a, Val::Int->new(('int' => 1)) );
    \@a
})), ('code' => 'infix:<->'), ('namespace' => '')) );
    \@a
})), ('code' => 'circumfix:<( )>'), ('namespace' => '')) );
    \@a
})), ('code' => 'infix:<..>'), ('namespace' => ''))), ('topic' => undef())) )
        }
        else {
            push(@s, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(64))) );
    push(@a, $item );
    \@a
})), ('code' => 'push'), ('namespace' => '')) )
        }
    };
    return (Do->new(('block' => Lit::Block->new(('sig' => undef()), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Decl->new(('decl' => 'my'), ('type' => ''), ('var' => Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(64))))) );
    push(@a, Decl->new(('decl' => 'my'), ('type' => ''), ('var' => Var->new(('name' => 'v'), ('namespace' => ''), ('sigil' => chr(64))))) );
    (@v = @s);
    for my $x ((0 .. ((scalar(@v) - 1)))) {
        push(@a, $v[$x] )
    };
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(64))) );
    \@a
})), ('code' => 'prefix:<' . chr(92) . '>'), ('namespace' => '')) );
    \@a
}))))))
};
package Lit::Hash;
sub expand_interpolation {
    ((my  $self) = $_[0]);
    (my  @items);
    for my $item (@{$self->{('hash1')}}) {
        if ((($item->isa('Apply') && ((($item->code() eq 'circumfix:<( )>') || ($item->code() eq 'list:<,>')))))) {
            for my $arg (@{$item->arguments()}) {
                push(@items, $arg )
            }
        }
        else {
            push(@items, $item )
        }
    };
    (my  @s);
    for my $item (@items) {
        if ((($item->isa('Apply') && ($item->code() eq 'infix:<' . chr(61) . '>>')))) {
            push(@s, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Lookup->new(('index_exp' => $item->arguments()->[0]), ('obj' => Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(36))))) );
    push(@a, $item->arguments()->[1] );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) )
        }
        else {
            if (((($item->isa('Var') && ($item->sigil() eq chr(37))) || ($item->isa('Apply') && ($item->code() eq 'prefix:<' . chr(37) . '>'))))) {
                ((my  $v) = $item);
                if ($item->isa('Var')) {
                    ($v = Var->new(('sigil' => chr(36)), ('namespace' => $item->namespace()), ('name' => $item->name())))
                };
                push(@s, For->new(('body' => Lit::Block->new(('sig' => Var->new(('name' => 'p'), ('namespace' => ''), ('sigil' => chr(36)))), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Lookup->new(('index_exp' => Var->new(('name' => 'p'), ('namespace' => ''), ('sigil' => chr(36)))), ('obj' => Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(36))))) );
    push(@a, Lookup->new(('index_exp' => Var->new(('name' => 'p'), ('namespace' => ''), ('sigil' => chr(36)))), ('obj' => $v)) );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
    \@a
})))), ('cond' => Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, $item );
    \@a
})), ('code' => 'keys'), ('namespace' => ''))), ('topic' => undef())) )
            }
            else {
                if (((($item->isa('Var') && ($item->sigil() eq chr(64))) || ($item->isa('Apply') && ($item->code() eq 'prefix:<' . chr(64) . '>'))))) {
                    push(@s, Do->new(('block' => Lit::Block->new(('sig' => undef()), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Decl->new(('decl' => 'my'), ('type' => ''), ('var' => Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36))))) );
    push(@a, Val::Int->new(('int' => 0)) );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Decl->new(('decl' => 'my'), ('type' => ''), ('var' => Var->new(('name' => '_a'), ('namespace' => ''), ('sigil' => chr(64))))) );
    push(@a, $item );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
    push(@a, While->new(('body' => Lit::Block->new(('sig' => undef()), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Lookup->new(('index_exp' => Index->new(('index_exp' => Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36)))), ('obj' => Var->new(('name' => '_a'), ('namespace' => ''), ('sigil' => chr(36)))))), ('obj' => Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(36))))) );
    push(@a, Index->new(('index_exp' => Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36))) );
    push(@a, Val::Int->new(('int' => 1)) );
    \@a
})), ('code' => 'infix:<+>'), ('namespace' => ''))), ('obj' => Var->new(('name' => '_a'), ('namespace' => ''), ('sigil' => chr(36))))) );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36))) );
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36))) );
    push(@a, Val::Int->new(('int' => 2)) );
    \@a
})), ('code' => 'infix:<+>'), ('namespace' => '')) );
    \@a
})), ('code' => 'infix:<' . chr(61) . '>'), ('namespace' => '')) );
    \@a
})))), ('cond' => Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => '_i'), ('namespace' => ''), ('sigil' => chr(36))) );
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => '_a'), ('namespace' => ''), ('sigil' => chr(64))) );
    \@a
})), ('code' => 'scalar'), ('namespace' => '')) );
    \@a
})), ('code' => 'infix:<<>'), ('namespace' => '')) );
    \@a
})), ('code' => 'circumfix:<( )>'), ('namespace' => '')))) );
    \@a
}))))) )
                }
                else {
                    die('Error in hash composer: ', $item)
                }
            }
        }
    };
    return (Do->new(('block' => Lit::Block->new(('sig' => undef()), ('stmts' => (do {
    (my  @a);
    (my  @v);
    push(@a, Decl->new(('decl' => 'my'), ('type' => ''), ('var' => Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(37))))) );
    (@v = @s);
    for my $x ((0 .. ((scalar(@v) - 1)))) {
        push(@a, $v[$x] )
    };
    push(@a, Apply->new(('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, Var->new(('name' => 'a'), ('namespace' => ''), ('sigil' => chr(37))) );
    \@a
})), ('code' => 'prefix:<' . chr(92) . '>'), ('namespace' => '')) );
    \@a
}))))))
};
package Apply;
((my  %op) = (('infix:<+' . chr(61) . '>' => 'infix:<+>'), ('infix:<-' . chr(61) . '>' => 'infix:<->'), ('infix:<*' . chr(61) . '>' => 'infix:<*>'), ('infix:<' . chr(47) . chr(61) . '>' => 'infix:<' . chr(47) . '>'), ('infix:<' . chr(124) . chr(124) . chr(61) . '>' => 'infix:<' . chr(124) . chr(124) . '>'), ('infix:<' . chr(38) . chr(38) . chr(61) . '>' => 'infix:<' . chr(38) . chr(38) . '>'), ('infix:<' . chr(124) . chr(61) . '>' => 'infix:<' . chr(124) . '>'), ('infix:<' . chr(38) . chr(61) . '>' => 'infix:<' . chr(38) . '>'), ('infix:<' . chr(47) . chr(47) . chr(61) . '>' => 'infix:<' . chr(47) . chr(47) . '>'), ('infix:<.' . chr(61) . '>' => 'list:<.>')));
sub op_assign {
    ((my  $self) = $_[0]);
    ((my  $code) = $self->{('code')});
    if (ref($code)) {
        return (0)
    };
    if ((exists($op{$code}))) {
        return (Apply->new(('code' => 'infix:<' . chr(61) . '>'), ('arguments' => (do {
    (my  @a);
    (my  @v);
    push(@a, $self->{('arguments')}->[0] );
    push(@a, Apply->new(('code' => $op{$code}), ('arguments' => $self->{('arguments')})) );
    \@a
}))))
    };
    return (0)
};
package Do;
sub simplify {
    ((my  $self) = $_[0]);
    (my  $block);
    if (($self->{('block')}->isa('Lit::Block'))) {
        ($block = $self->{('block')}->stmts())
    }
    else {
        ($block = (do {
    (my  @a);
    (my  @v);
    push(@a, $self->{('block')} );
    \@a
}))
    };
    if (((scalar(@{$block}) == 1))) {
        ((my  $stmt) = $block->[0]);
        if ((($stmt->isa('Apply') && ($stmt->code() eq 'circumfix:<( )>')))) {
            ((my  $args) = $stmt->arguments());
            return (Do->new(('block' => $args->[0]))->simplify())
        };
        if (($stmt->isa('Do'))) {
            return ($stmt->simplify())
        }
    };
    return (Do->new(('block' => $block)))
};

1;

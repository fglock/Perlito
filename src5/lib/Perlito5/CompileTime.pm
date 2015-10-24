use v5;
use Perlito5::AST;
use strict;

package Perlito5::CompileTime;
{
    sub emit_compile_time_block {
        my $block = $_[0];
        return [ 
                 map { defined($_) && $_->emit_compile_time() } @$block
               ];
    }
}

package Perlito5::AST::CompUnit;
{
    sub emit_compile_time {
        my $self = bless { %{$_[0]} }, ref($_[0]);
        $self->{body} = Perlito5::CompileTime::emit_compile_time_block( $self->{body} );
        return $self;
    }
    sub emit_compile_time_program {
        my $comp_units = $_[0];
        return  map { $_->emit_compile_time() } @$comp_units;
    }
}

package Perlito5::AST::Int;
{
    sub emit_compile_time {
        return $_[0];
    }
}

package Perlito5::AST::Num;
{
    sub emit_compile_time {
        return $_[0];
    }
}

package Perlito5::AST::Buf;
{
    sub emit_compile_time {
        return $_[0];
    }
}

package Perlito5::AST::Block;
{
    sub emit_compile_time {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{name}) {
            push @out, [ stmt => [ keyword => $self->{name} ], Perlito5::Perl5::emit_compile_time_block($self->{stmts}) ];
        }
        else {
            push @out, Perlito5::Perl5::emit_compile_time_block($self->{stmts});
        }
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_compile_time_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Index;
{
    sub emit_compile_time {
        my $self = $_[0];
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
              )
           )
        {
            return [ apply => '[', $self->{obj}->emit_compile_time(), $self->{index_exp}->emit_compile_time() ];
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '%' )
              )
           )
        {
            return [ apply => '[', $self->{obj}->emit_compile_time(), $self->{index_exp}->emit_compile_time() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return [ op => 'infix:<->>', $self->{obj}{arguments}[0]->emit_compile_time(), 
                     [ op => 'circumfix:<[ ]>', $self->{index_exp}->emit_compile_time() ] ];
        }
        return [ op => 'infix:<->>', $self->{obj}->emit_compile_time(), 
                 [ op => 'circumfix:<[ ]>', $self->{index_exp}->emit_compile_time() ] ];
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_compile_time {
        my $self = $_[0];
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
              )
           )
        {
            return [ apply => '{', $self->{obj}->emit_compile_time(), $self->autoquote($self->{index_exp})->emit_compile_time() ];
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '%' )
              )
           )
        {
            # perl5.20:  %a{ x, y }
            return [ apply => '{', $self->{obj}->emit_compile_time(), $self->autoquote($self->{index_exp})->emit_compile_time() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return [ op => 'infix:<->>', $self->{obj}{arguments}[0]->emit_compile_time(), 
                     [ op => 'circumfix:<{ }>', $self->autoquote($self->{index_exp})->emit_compile_time() ] ];
        }
        return [ op => 'infix:<->>', $self->{obj}->emit_compile_time(), 
                 [ op => 'circumfix:<{ }>', $self->autoquote($self->{index_exp})->emit_compile_time() ] ];
    }
}

package Perlito5::AST::Var;
{
    sub emit_compile_time {
        my $self = bless { %{$_[0]} }, ref($_[0]);
        if ($self->{_decl} eq 'my') {
            $self->{_decl} = 'global';
            $self->{namespace} = 'MY';
        }
        return $self;
    }
}

package Perlito5::AST::Call;
{
    sub emit_compile_time {
        my $self = $_[0]; 
        my $invocant = $self->{invocant}->emit_compile_time();
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return [ op => 'infix:<->>', $invocant, 
                     [ op => 'circumfix:<[ ]>', $self->{arguments}->emit_compile_time() ] ];
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return [ op => 'infix:<->>', $invocant, 
                     [ op => 'circumfix:<{ }>', Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_compile_time() ] ];
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
            if (  (  ref($self->{invocant}) eq 'Perlito5::AST::Var'
                  && $self->{invocant}{sigil} eq '&'
                  )
               || (  ref($self->{invocant}) eq 'Perlito5::AST::Apply'
                  && $self->{invocant}{code} eq 'prefix:<&>'
                  )
               ) 
            {
                #  &subr(args)
                return [ apply => '(', $invocant, map { $_->emit_compile_time() } @{$self->{arguments}} ];
            }
            $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_compile_time();
        }
        if ( $meth ) {
            return [ call => $invocant, $meth, map { $_->emit_compile_time() } @{$self->{arguments}} ];
        }
        return [ op => 'infix:<->>', $invocant, [ op => 'list:<,>', map { $_->emit_compile_time() } @{$self->{arguments}} ] ];
    }
}

package Perlito5::AST::Apply;
{
    sub emit_compile_time_args {
        my $self = $_[0];
        return () if !$self->{arguments};
        return map { $_->emit_compile_time() } @{$self->{arguments}};
    }
    sub emit_compile_time {
        my $self = $_[0];   
        if (ref $self->{code}) {
            return [ op => 'infix:<->>', $self->{code}->emit_compile_time(), $self->emit_compile_time_args() ];
        }
        if ($self->{code} eq 'infix:<=>>')  { 
            return [ op => $self->{code}, 
                     Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_compile_time(),
                     $self->{arguments}[1]->emit_compile_time() ]
        }
        if ( $Perlito5::Perl5::PrettyPrinter::op{ $self->{code} } ) {
            return [ op => $self->{code}, $self->emit_compile_time_args() ];
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if ($self->{code} eq 'p5:s') {
            return 's!' . $self->{arguments}->[0]->{buf}   # emit_compile_time() 
                 .  '!' . $self->{arguments}->[1]->{buf}   # emit_compile_time()
                 .  '!' . $self->{arguments}->[2]->{buf};

        }
        if ($self->{code} eq 'p5:m') {
            my $s;
            if ($self->{arguments}->[0]->isa('Perlito5::AST::Buf')) {
                $s = $self->{arguments}->[0]->{buf}
            }
            else {
                for my $ast (@{$self->{arguments}[0]{arguments}}) {
                    if ($ast->isa('Perlito5::AST::Buf')) {
                        $s .= $ast->{buf}
                    }
                    else {
                        $s .= $ast->emit_compile_time();  # variable name
                    }
                }
            }

            return 'm!' . $s . '!' . $self->{arguments}->[1]->{buf};
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_compile_time() 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_compile_time()
                 .   '!';
        }

        if ($self->{code} eq 'package')    { return [ stmt => 'package', [ bareword => $self->{namespace} ] ] }

        if ($code eq 'map' || $code eq 'grep' || $code eq 'sort') {    
            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return [ op => 'prefix:<' . $code . '>',
                         [ 'block', map { $_->emit_compile_time() } @{$self->{special_arg}{stmts}} ],
                         [ 'op' => 'list:<,>',  $self->emit_compile_time_args() ],
                       ];
            }
            return [ apply => '(', $code, $self->emit_compile_time_args() ];
        }

        if (($code eq 'eval' || $code eq 'do') && ref($self->{'arguments'}->[0]) eq 'Perlito5::AST::Block') {
            return ['op' => 'prefix:<' . $code . '>', $self->{'arguments'}->[0]->emit_compile_time()]
        }

        if ($code eq 'readline') {
            return [ paren => '<', $self->emit_compile_time_args() ];
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return [ bareword => $code ];
        }
        return [ apply => '(', $code, $self->emit_compile_time_args() ];
    }
}

package Perlito5::AST::If;
{
    sub emit_compile_time {
        my $self = $_[0]; 
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{body}->emit_compile_time(),
                                      [ stmt => 'if', $self->{cond}->emit_compile_time() ] ];
        }
        if ($self->{otherwise} && ref($self->{otherwise}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{otherwise}->emit_compile_time(),
                                      [ stmt => 'unless', $self->{cond}->emit_compile_time() ] ];
        }
        my @out = ( [ stmt => [ keyword => 'if' ],
                      [ paren => '(', $self->{cond}->emit_compile_time() ],
                      Perlito5::Perl5::emit_compile_time_block($self->{body}->stmts)
                    ] );
        my $otherwise = $self->{otherwise};

        while ( $otherwise
              && @{ $otherwise->{stmts} } == 1 
              && ref($otherwise->{stmts}[0]) eq 'Perlito5::AST::If'
              && ($otherwise->{stmts}[0]{body} && ref($otherwise->{stmts}[0]{body}) eq 'Perlito5::AST::Block')
              )
        {
            push @out, [ stmt => [ keyword => 'elsif' ],
                         [ paren => '(', $otherwise->{stmts}[0]{cond}->emit_compile_time() ],
                         Perlito5::Perl5::emit_compile_time_block($otherwise->{stmts}[0]{body}{stmts})
                       ];
            $otherwise = $otherwise->{stmts}[0]{otherwise};
        }

        return @out if !($otherwise && scalar(@{ $otherwise->stmts }));

        push @out, [ stmt => [ keyword => 'else' ],
                     Perlito5::Perl5::emit_compile_time_block($otherwise->stmts)
                   ];
        return @out;
    }
}

package Perlito5::AST::When;
{
    sub emit_compile_time {
        my $self = $_[0];
        return [ stmt => [ keyword => 'when' ],
                 [ paren => '(', $self->{cond}->emit_compile_time() ],
                 Perlito5::Perl5::emit_compile_time_block($self->{body}->stmts)
               ];
    }
}


package Perlito5::AST::While;
{
    sub emit_compile_time {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_compile_time(),
                                      [ stmt => [ keyword => 'while' ], $self->{cond}->emit_compile_time() ] ];
        }
        push @out, [ stmt => [ keyword => 'while' ],
                     [ paren => '(', $self->{cond}->emit_compile_time() ],
                     Perlito5::Perl5::emit_compile_time_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_compile_time_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::For;
{
    sub emit_compile_time {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        

        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_compile_time(),
                                      [ stmt => 'for', $self->{cond}->emit_compile_time() ] ];
        }

        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond = [ paren_semicolon => '(', 
                      ( $self->{cond}[0] ? $self->{cond}[0]->emit_compile_time() : [] ),
                      ( $self->{cond}[1] ? $self->{cond}[1]->emit_compile_time() : [] ),
                      ( $self->{cond}[2] ? $self->{cond}[2]->emit_compile_time() : [] ),
                    ];
        }
        else {
            $cond = [ paren => '(', $self->{cond}->emit_compile_time() ];
        }

        my @sig;
        my $sig_ast = $self->{topic};
        if (!$sig_ast) {
            # $_
        }
        else {
            @sig = $sig_ast->emit_compile_time();
        }
        push @out, [ stmt => [ keyword => 'for' ],
                     @sig,
                     $cond,
                     Perlito5::Perl5::emit_compile_time_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_compile_time_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Decl;
{
    sub emit_compile_time {
        my $self = $_[0];
        return $self->{var}->emit_compile_time();
    }
}

package Perlito5::AST::Sub;
{
    sub emit_compile_time {
        my $self = $_[0];
        my @parts;
        push @parts, [ paren => '(', [ bareword => $self->{sig} ] ]
            if defined $self->{sig};
        push @parts, Perlito5::Perl5::emit_compile_time_block($self->{block}{stmts})
            if defined $self->{block};
        return [ op => 'prefix:<sub>', @parts ] if !$self->{name};
        return [ stmt => [ keyword => 'sub' ], [ bareword => $self->{namespace} . "::" . $self->{name} ], @parts ];
    }
}

package Perlito5::AST::Use;
{
    sub emit_compile_time {
        my $self = shift;
        Perlito5::Grammar::Use::emit_time_eval($self);
        if ($Perlito5::EMIT_USE) {
            return [ stmt => [ keyword => 'use' ], [ bareword => $self->{mod} ] ];
        }
        else {
            return [ comment => "# " . $self->{code} . " " . $self->{mod} ];
        }
    }
}

1;

=begin

=head1 NAME

Perlito5::CompileTime - Compile-time AST generator for Perlito5

=head1 SYNOPSIS

    $program->emit_compile_time()  # generated compile-time AST

=head1 DESCRIPTION

This module generates compile-time AST for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012, 2015 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

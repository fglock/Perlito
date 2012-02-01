use v5;

use Perlito5::AST;

package Perl5;
{
    sub tab {
        my $level = shift;
        "    " x $level
    }

    my %safe_char = (
        '_' => 1,
        ',' => 1,
        '.' => 1,
        ':' => 1,
        '-' => 1,
        '+' => 1,
        '*' => 1,
        ' ' => 1,
        '(' => 1,
        ')' => 1,
        '<' => 1,
        '>' => 1,
        '[' => 1,
        ']' => 1,
    );

    sub escape_string {
        my $s = shift;
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for my $i (0 .. length($s) - 1) {
            my $c = substr($s, $i, 1);
            if  (  ($c ge 'a' && $c le 'z')
                || ($c ge 'A' && $c le 'Z')
                || ($c ge '0' && $c le '9')
                || exists( $safe_char{$c} )
                )
            {
                $tmp = $tmp . $c;
            }
            else {
                push @out, "'$tmp'" if $tmp ne '';
                push @out, "chr(" . ord($c) . ")";
                $tmp = '';
            }
        }
        push @out, "'$tmp'" if $tmp ne '';
        return join(' . ', @out);
    }

}

package CompUnit;
{
    sub emit_perl5 {
        $_[0]->emit_perl5_indented(0)
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];

        my @body;
        for (@{$self->{"body"}}) {
            if (defined($_)) {
                push @body, $_
            }
        }
          Perl5::tab($level)    . "\{\n"
        . Perl5::tab($level)    . 'package ' . $self->{"name"} . ";" . "\n"
        . Perl5::tab($level + 1)    . 'sub new { shift; bless { @_ }, "' . $self->{"name"} . '" }'  . "\n"
        .                             join(";\n", map( $_->emit_perl5_indented( $level + 1 ), @body )) . "\n"
        . Perl5::tab($level)    . "}\n"
        . "\n"
    }
    sub emit_perl5_program {
        my $comp_units = $_[0];

        my $str = ''
            . "use v5;\n"
            . "use utf8;\n"
            . "use strict;\n"
            . "use warnings;\n"
            . "no warnings ('redefine', 'once', 'void', 'uninitialized', 'misc', 'recursion');\n"
            . "use Perlito5::Perl5::Runtime;\n"
            . "our \$MATCH = Perlito5::Match->new();\n";
        for my $comp_unit (@{$comp_units}) {
            $str .= $comp_unit->emit_perl5_indented(0)
        }
        $str .= "1;\n";
        return $str;
    }
}

package Val::Int;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
         Perl5::tab($level) . $self->{"int"} }
}

package Val::Num;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
         Perl5::tab($level) . $self->{"num"} }
}

package Val::Buf;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        Perl5::tab($level) . Perl5::escape_string($self->{"buf"})
    }
}

package Lit::Block;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
          Perl5::tab($level) . "sub \{\n"
        .   join(";\n", map( $_->emit_perl5_indented( $level + 1 ), @{$self->{"stmts"}} )) . "\n"
        . Perl5::tab($level) . "}"
    }
}

package Lit::Array;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $ast = $self->expand_interpolation;
        return $ast->emit_perl5_indented($level);
    }
}

package Lit::Hash;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $ast = $self->expand_interpolation;
        return $ast->emit_perl5_indented($level);
    }
}

package Index;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '@', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_perl5_indented($level) . '->[' . $self->{"index_exp"}->emit_perl5() . ']';
        }

        $self->{"obj"}->emit_perl5_indented($level) . '->[' . $self->{"index_exp"}->emit_perl5() . ']';
    }
}

package Lookup;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{"obj"}->isa('Var')
           && $self->{"obj"}->sigil eq '$'
           && $self->{"obj"}->name ne 'MATCH'  # XXX $MATCH is the Perl6 match object
           )
        {
            my $v = Var->new( sigil => '%', namespace => $self->{"obj"}->namespace, name => $self->{"obj"}->name );
            return $v->emit_perl5_indented($level) . '->{' . $self->{"index_exp"}->emit_perl5() . '}';
        }

        $self->{"obj"}->emit_perl5_indented($level) . '->{' . $self->{"index_exp"}->emit_perl5() . '}';
    }
}

package Var;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        # Normalize the sigil
        my $table = {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        }
        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = $self->{"namespace"} . '::';
        }
        else {
            if ($self->{"sigil"} eq '@' && $self->{"name"} eq 'ARGV') {
                return Perl5::tab($level) . '(\\@ARGV)'
            }
        }
        return Perl5::tab($level) . $table->{$self->{"sigil"}} . $ns . $self->{"name"}
    }
    sub plain_name {
        my $self = $_[0];

        if ($self->{"namespace"}) {
            return $self->{"namespace"} . '::' . $self->{"name"}
        }
        return $self->{"name"}
    }
}

package Proto;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        Perl5::tab($level) . $self->{"name"}
    }
}

package Call;
{

    my %method_perl5 = (
        'isa'    => 'Perlito5::Runtime::isa',
    );

    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $invocant = $self->{"invocant"}->emit_perl5;

        if (exists( $method_perl5{ $self->{"method"} } )) {
            return Perl5::tab($level)
                . $method_perl5{ $self->{"method"} } . '(' . $invocant . ', ' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
        }
        
        if ( $self->{"method"} eq 'postcircumfix:<[ ]>' ) {
            return Perl5::tab($level) . $invocant . '->[' . $self->{"arguments"}->emit_perl5() . ']'
        }
        if ( $self->{"method"} eq 'postcircumfix:<{ }>' ) {
            return Perl5::tab($level) . $invocant . '->{' . $self->{"arguments"}->emit_perl5() . '}'
        }

        my $meth = $self->{"method"};
        if  ($meth eq 'postcircumfix:<( )>')  {
             $meth = '';
        }

        Perl5::tab($level) . $invocant . '->' . $meth . '(' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
    }
}

package Apply;
{

    my %op_prefix_perl5 = (
        say     => 'Perlito5::Runtime::say',
        print   => 'Perlito5::Runtime::print',
        grep    => 'Perlito5::Runtime::grep',
        sort    => 'Perlito5::Runtime::sort',
        keys    => 'Perlito5::Runtime::keys',
        values  => 'Perlito5::Runtime::values',
        warn    => 'warn',
        'prefix:<!>'    => '!',
        'prefix:<++>'   => '++',
        'prefix:<-->'   => '--',
        'prefix:<+>'    => '+',
    );

    my %op_infix_perl5 = (
        'list:<.>'   => ' . ',
        'infix:<+>'  => ' + ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',
        'infix:<x>'  => ' x ',

        'infix:<&&>' => ' && ',
        'infix:<||>' => ' || ',
        'infix:<and>' => ' and ',
        'infix:<or>' => ' or ',
        'infix:<//>' => ' // ',
        'infix:<eq>' => ' eq ',
        'infix:<ne>' => ' ne ',
        'infix:<le>' => ' le ',
        'infix:<ge>' => ' ge ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
        'infix:<=>>' => ' => ',
    );

    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_perl5_indented( $level );
        }

        my $ns = '';
        if ($self->{"namespace"}) {
            $ns = $self->{"namespace"} . '::';
        }
        my $code = $ns . $self->{"code"};

        if (ref $code ne '') {
            return Perl5::tab($level) . '(' . $self->{"code"}->emit_perl5() . ')->(' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
        }

        if (exists $op_infix_perl5{$code}) {
            return Perl5::tab($level) . '(' 
                . join( $op_infix_perl5{$code},
                        (map( $_->emit_perl5, @{$self->{"arguments"}} ))
                      )
                . ')'
        }
        if (exists $op_prefix_perl5{$code}) {
            return Perl5::tab($level) . $op_prefix_perl5{$code} . '('   . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')'
        }

        if ($self->{"code"} eq 'package')   { return Perl5::tab($level) . 'package ' . $self->{"namespace"} }
        if ($code eq 'undef')      { return Perl5::tab($level) . 'undef()' }

        if ($code eq 'scalar')     { return Perl5::tab($level) . 'scalar( @{' . ($self->{"arguments"}->[0]->emit_perl5) . '} )' }
        if ($code eq 'pop')        { return Perl5::tab($level) . 'pop( @{' . ($self->{"arguments"}->[0]->emit_perl5) . '} )' }
        if ($code eq 'push')       { return Perl5::tab($level) . 'push( @{' . ($self->{"arguments"}->[0])->emit_perl5() . '}, ' . ($self->{"arguments"}->[1])->emit_perl5() . ' )' }
        if ($code eq 'shift')      { 
            if ( @{$self->{"arguments"}} ) {
                return Perl5::tab($level) . 'shift( @{' . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))    . '} )' 
            }
            return 'shift()'
        }
        if ($code eq 'unshift')    { return Perl5::tab($level) . 'unshift( @{' . $self->{"arguments"}->[0]->emit_perl5()  . '}, ' . $self->{"arguments"}->[1]->emit_perl5() . ' )' }

        if ($code eq 'map')       {    
            my $str = shift @{$self->{"arguments"}};
            return Perl5::tab($level) . '[map(' . $str->emit_perl5 . ', @{' . join(',', map( $_->emit_perl5, @{$self->{"arguments"}} )) . '})]'
        }

        if ($code eq 'join')       {    
            my $str = shift @{$self->{"arguments"}};
            return Perl5::tab($level) . 'join(' . $str->emit_perl5 . ', @{' . join(',', map( $_->emit_perl5, @{$self->{"arguments"}} )) . '})'
        }

        if ($code eq 'prefix:<\\>') { 
            # XXX currently a no-op
            return Perl5::tab($level) . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} )) 
        }
        if ($code eq 'prefix:<$>') { return Perl5::tab($level) . '${' . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))     . '}' }
        if ($code eq 'prefix:<@>') { return Perl5::tab($level) . '(' . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))     . ')' }
        if ($code eq 'prefix:<%>') { return Perl5::tab($level) . '%{' . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))     . '}' }

        if ($code eq 'postfix:<++>') { return Perl5::tab($level) . '('   . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return Perl5::tab($level) . '('   . join(' ', map( $_->emit_perl5, @{$self->{"arguments"}} ))  . ')--' }

        if ($code eq 'infix:<..>') { return Perl5::tab($level) . '(bless ['  . join(' .. ', map( $_->emit_perl5, @{$self->{"arguments"}} ))  . "], 'ARRAY')" }

        if ($code eq 'ternary:<?? !!>') {
            return Perl5::tab($level)
                .  '('  . $self->{"arguments"}->[0]->emit_perl5
                . ' ? ' . $self->{"arguments"}->[1]->emit_perl5
                . ' : ' . $self->{"arguments"}->[2]->emit_perl5
                .  ')'
        }

        if ($code eq 'circumfix:<( )>') {
            return Perl5::tab($level) . '(' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return Perl5::tab($level) . emit_perl5_bind( $self->{"arguments"}->[0], $self->{"arguments"}->[1] );
        }
        if ($code eq 'return') {
            return Perl5::tab($level) . 'return (' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
        }

        Perl5::tab($level) . $code . '(' . join(', ', map( $_->emit_perl5, @{$self->{"arguments"}} )) . ')';
    }

    sub emit_perl5_bind {
        my $parameters = shift;
        my $arguments = shift;

        if ($parameters->isa( 'Call' )) {

            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>'
                || $parameters->method eq 'postcircumfix:<[ ]>'
                )
            {
                return '(' . $parameters->emit_perl5() . ' = ' . $arguments->emit_perl5() . ')';
            }

        }
        if      $parameters->isa( 'Var' ) && $parameters->sigil eq '@'
            ||  $parameters->isa( 'Decl' ) && $parameters->var->sigil eq '@'
        {
            $arguments = Lit::Array->new( array1 => [$arguments] );
        }
        elsif   $parameters->isa( 'Var' ) && $parameters->sigil eq '%'
            ||  $parameters->isa( 'Decl' ) && $parameters->var->sigil eq '%'
        {
            $arguments = Lit::Hash->new( hash1 => [$arguments] );
        }
        '(' . $parameters->emit_perl5() . ' = ' . $arguments->emit_perl5() . ')';
    }
}

package If;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        return Perl5::tab($level) . 'if (' . $self->{"cond"}->emit_perl5() . ") \{\n"
             .  ($self->{"body"}
                ? join(";\n", 
                       map( $_->emit_perl5_indented( $level + 1 ), $self->{"body"}->stmts )
                  ) . "\n"
                : ''
                )
             . Perl5::tab($level) . "}"
             .  ($self->{"otherwise"} && scalar(@{ $self->{"otherwise"}->stmts })
                ?  ( "\n"
                    . Perl5::tab($level) . "else \{\n"
                    .   join( ";\n", 
                              map( $_->emit_perl5_indented( $level + 1 ), $self->{"otherwise"}->stmts)
                        ) . "\n"
                    . Perl5::tab($level) . "}"
                    )
                : ''
                );
    }
}

package While;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{"cond"};
        if   $cond->isa( 'Var' )
          && $cond->sigil eq '@'
        {
            $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
           Perl5::tab($level) . 'for ( '
        .  ( $self->{"init"}     ? $self->{"init"}->emit_perl5()           . '; ' : '; ' )
        .  ( $cond      ? $cond->emit_perl5()            . '; ' : '; ' )
        .  ( $self->{"continue"} ? $self->{"continue"}->emit_perl5()       . ' '  : ' '  )
        .  ') {' . "\n"
        .       join(";\n", 
                     map( $_->emit_perl5_indented( $level + 1 ), $self->{"body"}->stmts )
                    ) . "\n"
        . Perl5::tab($level) . "}"
    }
}

package For;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{"cond"};
        if (!( $cond->isa( 'Var' ) && $cond->sigil eq '@' )) {
            $cond = Lit::Array->new( array1 => [$cond] )
        }
        my $sig;
        if ($self->{"body"}->sig()) {
            $sig = 'my ' . $self->{"body"}->sig->emit_perl5() . ' ';
        }
        return  Perl5::tab($level) . 'for ' . $sig . '( @{' . $cond->emit_perl5() . '} ) {' . "\n"
             .   join(";\n", map( $_->emit_perl5_indented( $level + 1 ), $self->{"body"}->stmts )) . "\n"
             . Perl5::tab($level) . "}"
    }
}

package Decl;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $decl = $self->{"decl"};
        my $name = $self->{"var"}->plain_name;
        my $str =
            '(' . $self->{"decl"} . ' ' . $self->{"type"} . ' ' . $self->{"var"}->emit_perl5();
        if ($self->{"var"})->sigil eq '%' {
            $str .= ' = bless {}, \'HASH\')';
        }
        elsif ($self->{"var"})->sigil eq '@' {
            $str .= ' = bless [], \'ARRAY\')';
        }
        else {
            $str .= ')';
        }
        return Perl5::tab($level) . $str;
    }
}

package Sub;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $sig = $self->{"sig"};
        my $pos = $sig->positional;
        my $str = '';
        my $i = 0;
        for my $field (@$pos) {
            $str .= Perl5::tab( $level + 1 ) . 'my ' . $field->emit_perl5() . ' = $_[' . $i . '];' . "\n";
            $i = $i + 1;
        }
          Perl5::tab($level) . 'sub ' . $self->{"name"} . " \{\n"
        . Perl5::tab( $level + 1 ) . 'my $List__ = bless \\@_, "ARRAY";' . "\n"
        .   $str
        .   join(";\n", map( $_->emit_perl5_indented( $level + 1 ), @{$self->{"block"}} )) . "\n"
        . Perl5::tab($level) . "}"
    }
}

package Do;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $block = $self->simplify->block;
          Perl5::tab($level) . "(do \{\n"
        .   join(";\n", map( defined($_) && $_->emit_perl5_indented( $level + 1 ), @$block )) . "\n"
        . Perl5::tab($level) . "})"
    }
}

package Use;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        if  (  $self->{"mod"} eq 'strict'
            || $self->{"mod"} eq 'feature'
            )
        {
            return "\n"
                . Perl5::tab($level) . "# use " . $self->{"mod"} . "\n"
        }
        Perl5::tab($level) . 'use ' . $self->{"mod"}
    }
}

=begin

=head1 NAME

Perlito5::Perl5::Emit - Code generator for Perlito5-in-Perl5

=head1 SYNOPSIS

    $program->emit_perl5()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Perl5 code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

use v5;
use Perlito5::AST;
use strict;

package Perlito5::Perl5;
{
    sub escape_string {
        return Perlito5::Dumper::escape_string($_[0]);
    }
    sub emit_perl5_block {
        my $block = $_[0];
        return [ 'block', 
                 map { defined($_) && $_->emit_perl5() } @$block
               ];
    }
}

package Perlito5::AST::CompUnit;
{
    sub emit_perl5 {
        my $self = $_[0];
        return [ 'block', (
                        [ stmt => [ keyword => 'package'], [ bareword => $self->{name} ] ],
                        map { defined($_) && $_->emit_perl5() } @{$self->{body}}
                    )
               ];
    }
    sub emit_perl5_program {
        my $comp_units = $_[0];
        return  
            [ comment => Perlito5::Compiler::do_not_edit("#") ],
            map { $_->emit_perl5() }
              map { ref($_) eq 'ARRAY' ? @$_ : $_ }
                @$comp_units;
    }
}

package Perlito5::AST::Int;
{
    sub emit_perl5 {
        my $self  = $_[0];
        [ number => $self->{int} ];
    }
}

package Perlito5::AST::Num;
{
    sub emit_perl5 {
        my $self  = $_[0];
        [ number => $self->{num} ];
    }
}

package Perlito5::AST::Buf;
{
    sub emit_perl5 {
        my $self  = $_[0];
        Perlito5::Perl5::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Block;
{
    sub emit_perl5 {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{name}) {
            push @out, [ stmt => [ keyword => $self->{name} ], Perlito5::Perl5::emit_perl5_block($self->{stmts}) ];
        }
        else {
            push @out, Perlito5::Perl5::emit_perl5_block($self->{stmts});
        }
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_perl5_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Index;
{
    sub emit_perl5 {
        my $self = $_[0];
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
              )
           )
        {
            return [ apply => '[', $self->{obj}->emit_perl5(), $self->{index_exp}->emit_perl5() ];
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '%' )
              )
           )
        {
            return [ apply => '[', $self->{obj}->emit_perl5(), $self->{index_exp}->emit_perl5() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return [ op => 'infix:<->>', $self->{obj}{arguments}[0]->emit_perl5(), 
                     [ op => 'circumfix:<[ ]>', $self->{index_exp}->emit_perl5() ] ];
        }
        return [ op => 'infix:<->>', $self->{obj}->emit_perl5(), 
                 [ op => 'circumfix:<[ ]>', $self->{index_exp}->emit_perl5() ] ];
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_perl5 {
        my $self = $_[0];
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
              )
           )
        {
            return [ apply => '{', $self->{obj}->emit_perl5(), $self->autoquote($self->{index_exp})->emit_perl5() ];
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
            return [ apply => '{', $self->{obj}->emit_perl5(), $self->autoquote($self->{index_exp})->emit_perl5() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return [ op => 'infix:<->>', $self->{obj}{arguments}[0]->emit_perl5(), 
                     [ op => 'circumfix:<{ }>', $self->autoquote($self->{index_exp})->emit_perl5() ] ];
        }
        return [ op => 'infix:<->>', $self->{obj}->emit_perl5(), 
                 [ op => 'circumfix:<{ }>', $self->autoquote($self->{index_exp})->emit_perl5() ] ];
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl5 {
        my $self = $_[0];

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = "\\'" if $str_name eq "'";     # escape $'

        # Normalize the sigil
        my $ns = '';
        if ($self->{namespace}) {
            return $self->{namespace} . '::'
                if $self->{sigil} eq '::';
            if ($self->{namespace} eq 'main' && substr($self->{name}, 0, 1) eq '^') {
                # don't add the namespace to special variables
                return $self->{sigil} . '{' . $self->{name} . '}'
            }
            else {
                $ns = $self->{namespace} . '::';
            }
        }
        my $c = substr($self->{name}, 0, 1);
        if (  ($c ge 'a' && $c le 'z')
           || ($c ge 'A' && $c le 'Z')
           || ($c eq '_')
           || ($self->{name} eq '/')
           ) 
        {
            return $self->{sigil} . $ns . $self->{name}
        }
        return $self->{sigil} . "{" . Perlito5::Perl5::escape_string( $ns . $str_name ) . "}";
    }
}

package Perlito5::AST::Call;
{
    sub emit_perl5 {
        my $self = $_[0]; 
        my $invocant = $self->{invocant}->emit_perl5();
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return [ op => 'infix:<->>', $invocant, 
                     [ op => 'circumfix:<[ ]>', $self->{arguments}->emit_perl5() ] ];
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return [ op => 'infix:<->>', $invocant, 
                     [ op => 'circumfix:<{ }>', Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_perl5() ] ];
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
                return [ apply => '(', $invocant, map { $_->emit_perl5() } @{$self->{arguments}} ];
            }
            $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_perl5();
        }
        if ( $meth ) {
            return [ call => $invocant, $meth, map { $_->emit_perl5() } @{$self->{arguments}} ];
        }
        return [ op => 'infix:<->>', $invocant, [ op => 'list:<,>', map { $_->emit_perl5() } @{$self->{arguments}} ] ];
    }
}

package Perlito5::AST::Apply;
{
    sub emit_perl5_args {
        my $self = $_[0];
        return () if !$self->{arguments};
        return map { $_->emit_perl5() } @{$self->{arguments}};
    }
    sub emit_perl5 {
        my $self = $_[0];   
        if (ref $self->{code}) {
            return [ op => 'infix:<->>', $self->{code}->emit_perl5(), $self->emit_perl5_args() ];
        }
        if ($self->{code} eq 'infix:<=>>')  { 
            return [ op => $self->{code}, 
                     Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_perl5(),
                     $self->{arguments}[1]->emit_perl5() ]
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if (  $code eq 'prefix:<$>'
           || $code eq 'prefix:<@>'
           || $code eq 'prefix:<%>'
           || $code eq 'prefix:<&>'
           || $code eq 'prefix:<*>'
           || $code eq 'prefix:<$#>')
        {
            my $arg = $self->{'arguments'}->[0];
            if (ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'do') {
                my $arg = $arg->{'arguments'}->[0];
                if (ref($arg) eq 'Perlito5::AST::Block') {
                    return ['op' => $code, $arg->emit_perl5() ];
                }
            }
            # if (ref($arg) eq 'Perlito5::AST::Apply') {
                $code =~ /<([^>]+)>/;
                my $cap = $1;
                return [ apply => '{', $cap, $arg->emit_perl5() ];
            # }
        }
        if ((  $code eq 'eval'
            || $code eq 'do'
            )
           && ref($self->{'arguments'}->[0]) eq 'Perlito5::AST::Block'
           )
        {
            return ['op' => 'prefix:<' . $code . '>', $self->{'arguments'}->[0]->emit_perl5()]
        }

        if ( $Perlito5::Perl5::PrettyPrinter::op{ $self->{code} } ) {
            return [ op => $self->{code}, $self->emit_perl5_args() ];
        }

        if ($self->{code} eq 'p5:s') {
            return 's!' . $self->{arguments}->[0]->{buf}   # emit_perl5() 
                 .  '!' . $self->{arguments}->[1]->{buf}   # emit_perl5()
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
                        $s .= $ast->emit_perl5();  # variable name
                    }
                }
            }

            return 'm!' . $s . '!' . $self->{arguments}->[1]->{buf};
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_perl5() 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_perl5()
                 .   '!';
        }

        if ($self->{code} eq 'package')    { return [ stmt => 'package', [ bareword => $self->{namespace} ] ] }

        if ($code eq 'map' || $code eq 'grep' || $code eq 'sort') {    
            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return [ op => 'prefix:<' . $code . '>',
                         [ 'block', map { $_->emit_perl5() } @{$self->{special_arg}{stmts}} ],
                         [ 'op' => 'list:<,>',  $self->emit_perl5_args() ],
                       ];
            }
            return [ apply => '(', $code, $self->emit_perl5_args() ];
        }

        if ($code eq 'eval' && $Perlito5::PHASE eq 'BEGIN') {
            # eval-string inside BEGIN block
            # we add some extra information to the data, to make things more "dumpable"
            return [ apply => '(', 'eval',
                         [ apply => '(', 'Perlito5::CompileTime::Dumper::generate_eval_string',
                            $self->emit_perl5_args() ]];
        }

        if ($code eq 'readline') {
            return [ paren => '<', $self->emit_perl5_args() ];
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return [ bareword => $code ];
        }
        return [ apply => '(', $code, $self->emit_perl5_args() ];
    }
}

package Perlito5::AST::If;
{
    sub emit_perl5 {
        my $self = $_[0]; 
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{body}->emit_perl5(),
                                      [ stmt => 'if', $self->{cond}->emit_perl5() ] ];
        }
        if ($self->{otherwise} && ref($self->{otherwise}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{otherwise}->emit_perl5(),
                                      [ stmt => 'unless', $self->{cond}->emit_perl5() ] ];
        }
        my @out = ( [ stmt => [ keyword => 'if' ],
                      [ paren => '(', $self->{cond}->emit_perl5() ],
                      Perlito5::Perl5::emit_perl5_block($self->{body}->stmts)
                    ] );
        my $otherwise = $self->{otherwise};

        while ( $otherwise
              && @{ $otherwise->{stmts} } == 1 
              && ref($otherwise->{stmts}[0]) eq 'Perlito5::AST::If'
              && ($otherwise->{stmts}[0]{body} && ref($otherwise->{stmts}[0]{body}) eq 'Perlito5::AST::Block')
              )
        {
            push @out, [ stmt => [ keyword => 'elsif' ],
                         [ paren => '(', $otherwise->{stmts}[0]{cond}->emit_perl5() ],
                         Perlito5::Perl5::emit_perl5_block($otherwise->{stmts}[0]{body}{stmts})
                       ];
            $otherwise = $otherwise->{stmts}[0]{otherwise};
        }

        return @out if !($otherwise && scalar(@{ $otherwise->stmts }));

        push @out, [ stmt => [ keyword => 'else' ],
                     Perlito5::Perl5::emit_perl5_block($otherwise->stmts)
                   ];
        return @out;
    }
}

package Perlito5::AST::When;
{
    sub emit_perl5 {
        my $self = $_[0];
        return [ stmt => [ keyword => 'when' ],
                 [ paren => '(', $self->{cond}->emit_perl5() ],
                 Perlito5::Perl5::emit_perl5_block($self->{body}->stmts)
               ];
    }
}


package Perlito5::AST::While;
{
    sub emit_perl5 {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_perl5(),
                                      [ stmt => [ keyword => 'while' ], $self->{cond}->emit_perl5() ] ];
        }
        push @out, [ stmt => [ keyword => 'while' ],
                     [ paren => '(', $self->{cond}->emit_perl5() ],
                     Perlito5::Perl5::emit_perl5_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_perl5_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::For;
{
    sub emit_perl5 {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        

        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_perl5(),
                                      [ stmt => 'for', $self->{cond}->emit_perl5() ] ];
        }

        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond = [ paren_semicolon => '(', 
                      ( $self->{cond}[0] ? $self->{cond}[0]->emit_perl5() : [] ),
                      ( $self->{cond}[1] ? $self->{cond}[1]->emit_perl5() : [] ),
                      ( $self->{cond}[2] ? $self->{cond}[2]->emit_perl5() : [] ),
                    ];
        }
        else {
            $cond = [ paren => '(', $self->{cond}->emit_perl5() ];
        }

        my @sig;
        my $sig_ast = $self->{topic};
        if (!$sig_ast) {
            # $_
        }
        else {
            @sig = $sig_ast->emit_perl5();
        }
        push @out, [ stmt => [ keyword => 'for' ],
                     @sig,
                     $cond,
                     Perlito5::Perl5::emit_perl5_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl5::emit_perl5_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Decl;
{
    sub emit_perl5 {
        my $self = $_[0];
        return [ op => 'prefix:<' . $self->{decl} . '>', 
                 ($self->{type} ? $self->{type} : ()),
                 $self->{var}->emit_perl5()
               ];
    }
}

package Perlito5::AST::Sub;
{
    sub emit_perl5 {
        my $self = $_[0];
        my @sig;
        my @parts;
        # Note: enable this to perform state() vars macro substitution
        # if (my $node = $self->maybe_rewrite_statevars()) {
        #     return $node->emit_perl5(@_[1..$#_]);
        # }
        push @sig, [ paren => '(', [ bareword => $self->{sig} ] ]
            if defined $self->{sig};

        if (defined $self->{block}) {
            # this is not a pre-declaration
            push @parts, Perlito5::Perl5::emit_perl5_block($self->{block}{stmts});

            if ($Perlito5::PHASE eq 'BEGIN') {
                # at compile-time only:
                #   we are compiling - maybe inside a BEGIN block
                #   provide a way to dump this closure

                # get list of captured variables, including inner blocks
                my @captured;
                for my $stmt (@{$self->{block}{stmts}}) {
                    push @captured, $stmt->get_captures();
                }
                my %dont_capture = map { $_->{dont} ? ( $_->{dont} => 1 ) : () } @captured;
                my %capture = map { $_->{dont} ? ()
                                  : $dont_capture{ $_->{_id} } ? ()
                                  : ($_->{_decl} eq 'local' || $_->{_decl} eq 'global' || $_->{_decl} eq '') ? ()
                                  : ( $_->{_id} => $_ )
                                  } @captured;
                my @captures_ast  = values %capture;
                my @captures_perl = map {
                        ($_->{_real_sigil} || $_->{sigil}) . $_->{name}
                    } @captures_ast;

                my @extra;
                # return a hash with { "variable name" => \"variable value" }
                # with all captured variables
                # 
                #   @_ && ref($_[0]) eq "Perlito5::dump" && return "do { my \$x = $x; sub { \$_[0] + \$x;  } }" }
                #   @_ && ref($_[0]) eq "Perlito5::dump" && return { '$x' => \$x }



                # retrieve the source code for this sub
                my $code;
                {
                    local $Perlito5::PHASE = '';
                    my @data = [ op => 'prefix:<sub>', @sig, @parts ];
                    my $out = [];
                    Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                    $code = "package $Perlito5::PKG_NAME;\n"
                             . join( '', @$out );
                    # say "BEGIN block: [[ $code ]]";
                    $code = Perlito5::Perl5::escape_string($code);
                    # say "BEGIN block: [[ $code ]]";
                }


                push @extra,
                  [
                    'op', 'infix:<&&>',
                    '@_',
                    [
                        'op', 'infix:<&&>',
                        [
                            'op', 'infix:<eq>',
                            [ 'apply', '(', 'ref', [ 'apply', '[', '$_', [ 'number', 0, ], ], ],
                            '"Perlito5::dump"',
                        ],
                        [
                            'apply', '(', 'return',
                            [
                                'op', 'circumfix:<{ }>',
                                [ 'op', 'infix:<=>>', "'__SUB__'", $code ],
                                map { [ 'op', 'infix:<=>>', "'$_'", [ 'op', 'prefix:<\\>', $_ ] ], }
                                  @captures_perl
                            ],
                        ]
                    ]
                  ];

                my $bl = shift @{$parts[0]};
                unshift @{$parts[0]}, $bl, @extra;
            }
        }

        return [ op => 'prefix:<sub>', @sig, @parts ] if !$self->{name};
        return [ stmt => [ keyword => 'sub' ], [ bareword => $self->{namespace} . "::" . $self->{name} ], @sig, @parts ];
    }
}

package Perlito5::AST::Use;
{
    sub emit_perl5 {
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

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

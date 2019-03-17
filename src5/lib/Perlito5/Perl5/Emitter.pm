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
        if ($self->{is_vstring}) {
            return join(".", map { ord($_) } split( //, $self->{buf} ));
        }
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
            return [ apply => '{', $self->{obj}->emit_perl5(), $self->{index_exp}->emit_perl5() ];
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
            return [ apply => '{', $self->{obj}->emit_perl5(), $self->{index_exp}->emit_perl5() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return [ op => 'infix:<->>', $self->{obj}{arguments}[0]->emit_perl5(), 
                     [ op => 'circumfix:<{ }>', $self->{index_exp}->emit_perl5() ] ];
        }
        return [ op => 'infix:<->>', $self->{obj}->emit_perl5(), 
                 [ op => 'circumfix:<{ }>', $self->{index_exp}->emit_perl5() ] ];
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl5 {
        my $self = $_[0];
        $self = $self->to_begin_scratchpad();

        my $str_name = $self->{name};
        my $c = substr($str_name, 0, 1);

        if ($c ne "" && $c lt " " && $self->{sigil} ne "::") {
            return $self->{sigil} . "{^" . chr( ord($c) + ord("A") - 1 ) . substr($str_name, 1) . "}";
        }

        # Normalize the sigil
        my $ns = '';
        if ($self->{namespace}) {
            return $self->{namespace} . '::'
                if $self->{sigil} eq '::';
            if ($self->{namespace} eq 'main' && substr($str_name, 0, 1) eq '^') {
                # don't add the namespace to special variables
                return $self->{sigil} . '{' . $str_name . '}'
            }
            else {
                $ns = $self->{namespace} . '::';
            }
        }

        if (  ($c ge 'a' && $c le 'z')
           || ($c ge 'A' && $c le 'Z')
           || ($c eq '_')
           || ($str_name eq '/' || $str_name eq '&')
           || ( (0 + $str_name) eq $str_name )  # numeric
           ) 
        {
            return $self->{sigil} . $ns . $str_name
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
                     [ op => 'circumfix:<{ }>', $self->{arguments}->emit_perl5() ] ];
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
            $meth = '';
        }
        if ( ref($meth) ) {
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
    sub emit_perl5_choose_regex_quote {
        if (! grep { $_ =~ /\// } @_) {
            return "/";
        }
        if (! grep { $_ =~ /!/ } @_) {
            return "!";
        }
        if (! grep { $_ =~ /%/ } @_) {
            return "%";
        }
        if (! grep { $_ =~ /:/ } @_) {
            return ":";
        }
        if (! grep { $_ =~ /;/ } @_) {
            return ";";
        }
        return "^";
    }
    sub emit_perl5_regex_expression {
        my $ast = $_[0];
        if ($ast->isa('Perlito5::AST::Buf')) {
            my $replace = $ast->{buf};
            # $replace =~ s{\\}{\\\\}g;
            return $replace;
        }
        if ($ast->isa('Perlito5::AST::Apply') && $ast->{code} eq 'list:<.>') {
            # concatenate arguments
            my $s = "";
            for my $a (@{$ast->{arguments}}) {
                $s .= emit_perl5_regex_expression($a);
            }
            return $s;
        }
        # variable, lookup, index
        my $out = [];
        Perlito5::Perl5::PrettyPrinter::pretty_print([$ast->emit_perl5()], 0, $out);
        my $code = join('', @{$out});
        chomp $code;
        return $code;
    }
    sub emit_perl5 {
        my $self = $_[0];   
        if (ref $self->{code}) {

            my $code = $self->{code};
            if ( ref($code) eq 'Perlito5::AST::Apply' && $code->code eq "prefix:<&>") {
                # &$c()
                return [ apply => '(', $code->emit_perl5(), $self->emit_perl5_args() ];
            }

            return [ op => 'infix:<->>', $self->{code}->emit_perl5(), $self->emit_perl5_args() ];
        }
        if ($self->{code} eq 'list:<=>>')  { 
            return [ op => $self->{code}, 
                     $self->{arguments}[0]->emit_perl5(),
                     $self->{arguments}[1]->emit_perl5() ]
        }

        if ($self->{namespace} eq 'Perlito5') {
            # if ($self->{code} eq 'nop') {
            #     return ();
            # }
            if ($self->{code} eq 'eval_ast') {
                $self->{namespace} = 'Perlito5::Perl5::Runtime';
            }
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if ($code eq 'circumfix:<{ }>' && @{ $self->{'arguments'} } == 1) {
            # disambiguate block {1} from hash {"1"=>undef}
            return ['op', $code, $self->{'arguments'}->[0]->emit_perl5(), "" ];
        }

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
            my $replace0 = emit_perl5_regex_expression($self->{arguments}->[0]);
            my $replace1 = emit_perl5_regex_expression($self->{arguments}->[1]);
            my $q = emit_perl5_choose_regex_quote(
                        $replace0, 
                        $replace1,
                        $self->{arguments}->[2]->{buf}, 
                    );
            return 's' . $q . $replace0                        # emit_perl5() 
                 .       $q . $replace1                        # emit_perl5()
                 .       $q . $self->{arguments}->[2]->{buf};

        }
        if ($self->{code} eq 'p5:m') {
            my $replace0 = emit_perl5_regex_expression($self->{arguments}->[0]);
            my $q = emit_perl5_choose_regex_quote(
                        $replace0,
                        $self->{arguments}->[1]->{buf}, 
                    );
            return 'm' . $q . $replace0 . $q . $self->{arguments}->[1]->{buf};
        }
        if ($self->{code} eq 'p5:tr') {
            my $replace0 = emit_perl5_regex_expression($self->{arguments}->[0]);
            my $replace1 = emit_perl5_regex_expression($self->{arguments}->[1]);
            my $q = emit_perl5_choose_regex_quote(
                        $replace0, 
                        $replace1,
                        $self->{arguments}->[2]->{buf},
                    );
            return 'tr' . $q . $replace0                        # emit_perl5() 
                 .        $q . $replace1                        # emit_perl5()
                 .        $q . $self->{arguments}->[2]->{buf};
        }
        if ($self->{code} eq 'p5:qr') {
            my $replace0 = emit_perl5_regex_expression($self->{arguments}->[0]);
            my $q = emit_perl5_choose_regex_quote(
                        $replace0,
                        $self->{arguments}->[1]->{buf}, 
                    );
            return 'qr' . $q . $replace0 . $q . $self->{arguments}->[1]->{buf};
        }
        if ($self->{code} eq 'p5:format') {
            my @arg = @{ $self->{arguments} };
            my $name = shift @arg;
            $name = $name->{buf} if ref $name;
            my @str;
            push @str, "format " . ( $name ? $name . " " : "" ) . "=";
            while (@arg) {
                my $picture = shift @arg;
                push @str, $picture->{buf};
                my $ast = shift @arg;
                if ($ast) {
                    my $out = [];
                    Perlito5::Perl5::PrettyPrinter::pretty_print([$ast->emit_perl5()], 0, $out);
                    my $code = join('', @{$out});
                    chomp $code;
                    push @str, $code;
                }
            }
            push @str, ".\n"; 
            return join("\n", @str);
        }

        if ($self->{code} eq 'package')    { return [ stmt => 'package', [ bareword => $self->{namespace} ] ] }

        if ( $code eq 'map'
          || $code eq 'grep'
          || $code eq 'sort'
          || $code eq 'print'
          || $code eq 'use'
        ) {
            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return [ op => 'prefix:<' . $code . '>',
                         $self->{special_arg}->emit_perl5,
                         [ 'op' => 'list:<,>',  $self->emit_perl5_args() ],
                       ];
            }
            return [ apply => '(', $code, $self->emit_perl5_args() ];
        }

        if ($code eq 'readline') {
            return [ paren => '<', $self->emit_perl5_args() ];
        }

        $code = "&" . $code
            if $self->{ignore_proto};

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            my $effective_name = ($self->{namespace} || $Perlito5::PKG_NAME) . '::' . $self->{code};
            if (exists $Perlito5::PROTO->{$effective_name}) {
                $code = $effective_name;
            }
            else {
                # shift / push / return
                return ['bareword' => $code]
            }
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

        my $var = $self->{var};
        return $var->emit_perl5() if ref($var) eq 'Perlito5::AST::Var' && $var->is_begin_scratchpad();

        return [ op => 'prefix:<' . $self->{decl} . '>', 
                 ($self->{type} ? $self->{type} : ()),
                 $var->emit_perl5()
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
            push @parts, Perlito5::Perl5::emit_perl5_block( $self->{'block'}{stmts} );
        }

        return [ op => 'prefix:<sub>', @sig, @parts ] if !$self->{name};
        return [ stmt => [ keyword => 'sub' ], [ bareword => $self->{namespace} . "::" . $self->{name} ], @sig, @parts ];
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
The Pugs Team.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

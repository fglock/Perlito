use v5;

use Perlito5::AST;

package Perlito5::XS;
{
    sub tab {
        my $level = shift;
        "    " x $level
    }

    my %safe_char = (
        ' ' => 1,
        '!' => 1,
        '"' => 1,
        '#' => 1,
        '$' => 1,
        '%' => 1,
        '&' => 1,
        '(' => 1,
        ')' => 1,
        '*' => 1,
        '+' => 1,
        ',' => 1,
        '-' => 1,
        '.' => 1,
        '/' => 1,
        ':' => 1,
        ';' => 1,
        '<' => 1,
        '=' => 1,
        '>' => 1,
        '?' => 1,
        '@' => 1,
        '[' => 1,
        ']' => 1,
        '^' => 1,
        '_' => 1,
        '`' => 1,
        '{' => 1,
        '|' => 1,
        '}' => 1,
        '~' => 1,
    );

    sub escape_string {
        my $s = shift;
        my $tmp = '';
        return '""' if $s eq '';
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
                $tmp .= sprintf "\\x%02x", ord($c);
            }
        }
        return "\"$tmp\"";
    }

}

package Perlito5::AST::CompUnit;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];

        my @body;
        for (@{$self->{body}}) {
            if (defined($_)) {
                push @body, $_
            }
        }
        #   'MODULE = ' . $self->{name} . " PACKAGE = " . $self->{name} . "\n"
                 join("\n", map( Perlito5::XS::tab($level) . $_->emit_xs( $level ), @body )) . "\n"
        . "\n"
        . "\n"
    }
    sub emit_xs_program {
        my $comp_units = $_[0];
        # if ( $expand_use ) {
        #    print Perlito5::XS::Runtime->emit_xs();
        # }
        my $str =
                  Perlito5::Compiler::do_not_edit("/*")
                . "*/\n"
                . "#include \"EXTERN.h\"\n"
                . "#include \"perl.h\"\n"
                . "#include \"XSUB.h\"\n"
                . "\n";
        for my $comp_unit (@{$comp_units}) {
            $str .= $comp_unit->emit_xs(0)
        }
        $str .= "\n";
        return $str;
    }
}

package Perlito5::AST::Int;
{
    sub emit_xs {
        my $self  = $_[0];
        my $level = $_[1];
        'newSViv(' . $self->{int} . ')';
    }
}

package Perlito5::AST::Num;
{
    sub emit_xs {
        my $self  = $_[0];
        my $level = $_[1];
        'newSVnv(' . $self->{num} . ')';
    }
}

package Perlito5::AST::Buf;
{
    sub emit_xs {
        my $self  = $_[0];
        my $level = $_[1];
        'newSVpv(' . Perlito5::XS::escape_string( $self->{buf} ) . ', 0)';
    }
}

package Perlito5::AST::Block;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        # TODO - emit "continue" block

          "do {{\n"
        .   join(";\n", map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{$self->{stmts}} )) . "\n"
        . Perlito5::XS::tab($level) . "}}"
    }
}

package Perlito5::AST::Index;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->{sigil} eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_xs($level) . '[' . $self->{index_exp}->emit_xs($level+1) . ']';
        }

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return $self->{obj}{arguments}[0]->emit_xs($level) . '->[' . $self->{index_exp}->emit_xs($level) . ']';
        }

        $self->{obj}->emit_xs($level) . '->[' . $self->{index_exp}->emit_xs($level+1) . ']';
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->{sigil} eq '$'
           )
        {
            my $v = $self->{obj};
            return $v->emit_xs($level) . '{' . $self->{index_exp}->emit_xs($level) . '}';
        }

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a->{0}
            return $self->{obj}{arguments}[0]->emit_xs($level) . '->{' . $self->{index_exp}->emit_xs($level) . '}';
        }

        $self->{obj}->emit_xs($level) . '->{' . $self->{index_exp}->emit_xs($level) . '}';
    }
}

package Perlito5::AST::Var;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = '\\"' if $str_name eq '"';     # escape $"

        # Normalize the sigil
        my $ns = '';
        if (0 && $self->{namespace}) {
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
           ) 
        {
            return $self->{name};
        $self->{sigil} = '*';
            return $self->{sigil} . $ns . $self->{name}
        }
        return $self->{sigil} . "{'" . $ns . $str_name . "'}"
    }
}

package Perlito5::AST::Call;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        my $invocant = $self->{invocant}->emit_xs;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return $invocant . '->[' . $self->{arguments}->emit_xs($level+1) . ']'
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return $invocant . '->{' . $self->{arguments}->emit_xs($level) . '}'
        }
        my $meth = $self->{method};
        if  ($meth eq 'postcircumfix:<( )>')  {
             $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_xs($level+1);
        }
        $invocant . '->' . $meth . '(' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')';
    }
}

package Perlito5::AST::Apply;
{

    my %op_prefix_xs = (
        say     => 'say',
        #print   => 'print',
        keys    => 'keys',
        values  => 'values',
        #warn    => 'warn',
        'prefix:<!>'    => '!',
        'prefix:<++>'   => '++',
        'prefix:<-->'   => '--',
        'prefix:<+>'    => '+',
        'prefix:<->'    => '-',
        'prefix:<-d>'   => '-d',
        'prefix:<-e>'   => '-e',
        'prefix:<-f>'   => '-f',
        'prefix:<not>'  => 'not',
        'prefix:<~>'    => '~',
    );

    my %op_infix_xs = (
        'list:<,>'   => ', ',
        'list:<.>'   => ' . ',
        'infix:<+>'  => ' + ',
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<%>'  => ' % ',
        'infix:<**>' => ' ** ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',

        'infix:<&>'  => ' & ',
        'infix:<|>'  => ' | ',
        'infix:<^>'  => ' ^ ',
        'infix:<&&>' => ' && ',
        'infix:<||>' => ' || ',
        'infix:<and>' => ' and ',
        'infix:<or>' => ' or ',
        'infix:<//>' => ' // ',
        'infix:<eq>' => ' eq ',
        'infix:<ne>' => ' ne ',
        'infix:<le>' => ' le ',
        'infix:<ge>' => ' ge ',
        'infix:<lt>' => ' lt ',
        'infix:<gt>' => ' gt ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',

        'infix:<=~>' => ' =~ ',
        'infix:<!~>' => ' !~ ',
    );

    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_xs( $level );
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        my $code = $ns . $self->{code};

        if (ref $code ne '') {
            return '(' . $self->{code}->emit_xs($level+1) . ')->(' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')';
        }

        if (exists $op_infix_xs{$code}) {
            return '(' 
                . join( $op_infix_xs{$code},
                        (map( $_->emit_xs($level+1), @{$self->{arguments}} ))
                      )
                . ')'
        }
        if (exists $op_prefix_xs{$code}) {
            return $op_prefix_xs{$code} . '('   . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')'
        }

        if ($self->{code} eq 'p5:s') {
            return 's!' . $self->{arguments}->[0]->{buf}   # emit_xs($level+1) 
                 .  '!' . $self->{arguments}->[1]->{buf}   # emit_xs($level+1)
                 .  '!' . $self->{arguments}->[2];

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
                        $s .= $ast->emit_xs($level+1);  # variable name
                    }
                }
            }

            return 'm!' . $s . '!' . $self->{arguments}->[1];
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_xs($level+1) 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_xs($level+1)
                 .   '!';
        }

        if ($code eq '__PACKAGE__') {
            return '"' . $Perlito5::PKG_NAME . '"';
        }
        if ($self->{code} eq 'package')    { return 'MODULE = ' . $self->{namespace} . ' PACKAGE = ' . $self->{namespace} }
        if ($code eq 'undef')      { return 'undef()' }

        if ($code eq 'scalar')     { return 'scalar(' . ($self->{arguments}->[0]->emit_xs) . ')' }
        if ($code eq 'pop')        { return 'pop('  . ($self->{arguments}->[0]->emit_xs) . ')' }
        if ($code eq 'push')       { return 'push(' . ($self->{arguments}->[0]->emit_xs) . ', ' . ($self->{arguments}->[1])->emit_xs($level+1) . ' )' }
        if ($code eq 'shift')      { 
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return 'shift(' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))    . ')' 
            }
            return 'shift()'
        }
        if ($code eq 'unshift')    { return 'unshift(' . $self->{arguments}->[0]->emit_xs($level+1)  . ', ' . $self->{arguments}->[1]->emit_xs($level+1) . ')' }

        if ($code eq 'map')       {    

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return "map {\n"
                .   join(";\n", map { Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ) } @{$self->{special_arg}{stmts}} ) . "\n"
                . Perlito5::XS::tab($level) . "} "
    
                . join(',', map { $_->emit_xs($level+1) } @{$self->{arguments}} );
            }

            my $str = shift @{$self->{arguments}};
            return 'map(' . $str->emit_xs($level+1) . ', ' . join(',', map { $_->emit_xs($level+1) } @{$self->{arguments}} ) . ')'
        }

        if ($code eq 'infix:<x>')  { 
            return 'join("", ' . join(' x ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))  . ')'
        }
        if ($code eq 'list:<=>>')  { 
            return $self->{arguments}[0]->emit_xs($level)  . ', ' 
                . $self->{arguments}[1]->emit_xs($level)
        }

        if ($code eq 'join')       {    
            my $str = shift @{$self->{arguments}};
            return 'join(' . $str->emit_xs . ', ' . join(',', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')'
        }

        if ($code eq 'circumfix:<[ ]>') { 
            return '[' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ']'
        }
        if ($code eq 'circumfix:<{ }>') { 
            return '{' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . '}'
        }
        if ($code eq 'prefix:<\\>') { 
            # TODO - \(@a) vs. \@a
            return '\\' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ''
        }
        if ($code eq 'prefix:<$>') { return '${' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<@>') { return '@{' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<%>') { return '%{' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<&>') { return '&{' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))     . '}' }
        if ($code eq 'prefix:<$#>') { return '$#{' . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))     . '}' }


        if ($code eq 'postfix:<++>') { return '('   . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))  . ')++' }
        if ($code eq 'postfix:<-->') { return '('   . join(' ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))  . ')--' }

        if ($code eq 'infix:<..>') { return '('  . join(' .. ', map( $_->emit_xs($level+1), @{$self->{arguments}} ))  . ")" }

        if ($code eq 'ternary:<? :>') {
            return '('  . $self->{arguments}->[0]->emit_xs($level+1)
                . ' ? ' . $self->{arguments}->[1]->emit_xs($level+1)
                . ' : ' . $self->{arguments}->[2]->emit_xs($level+1)
                .  ')'
        }

        if ($code eq 'circumfix:<( )>') {
            return '(' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')';
        }
        if ($code eq 'infix:<=>') {
            return emit_xs_bind( $self->{arguments}->[0], $self->{arguments}->[1], $level );
        }

        if ($code eq 'require') {
            return 'Perlito5::Grammar::Use::require(' . $self->{arguments}[0]->emit_xs($level+1) . ')'
        }

        if ($code eq 'do') {
            # Note: this is "do EXPR" - look at the "Do" AST node for "do BLOCK"
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'slurp_file',
                          namespace => 'Perlito5::Grammar::Use',
                          arguments => $self->{arguments}
                        )
                    ]
                );
            return $ast->emit_xs( $level );
        }

        if ($code eq 'eval') {
            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Block" )) {
                $eval = $arg->emit_xs( $level + 1 );     # TODO -, $wantarray );
            }
            else {
                $eval = 
                    '(do { '
                    .   'my $m = Perlito5::Grammar::exp_stmts(' 
                    .       $arg->emit_xs( $level + 1, 'scalar' ) . ', 0);'

                    .   'my $source; '
                    .   '$source .= (defined $_ ? $_->emit_xs(0, "scalar") : "") . ";\n" '
                    .       'for @{ Perlito5::Match::flat($m) }; '

                    # .   'warn $source;'
                    .   'eval $source;'
                    . '})';
            }
            return $eval;
        }

        if ($code eq 'return') {
            return 'PUSHs(' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')';
        }

        if ($code eq 'print') {
            return 'puts( SvPVx_nolen( ' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ') )';
        }

        if ($code eq 'warn') {
            return 'warn( SvPVx_nolen( ' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ') )';
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return $code;
        }

        $code . '(' . join(', ', map( $_->emit_xs($level+1), @{$self->{arguments}} )) . ')';
    }

    sub emit_xs_bind {
        my $parameters = shift;
        my $arguments = shift;
        my $level = shift;

        if ($parameters->isa( 'Perlito5::AST::Call' )) {

            # $a->{3} = 4
            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>'
                || $parameters->method eq 'postcircumfix:<[ ]>'
                )
            {
                return '(' . $parameters->emit_xs($level+1) . ' = ' . $arguments->emit_xs($level+1) . ')';
            }

        }
        $parameters->emit_xs($level+1) . ' = ' . $arguments->emit_xs($level+1);
    }
}

package Perlito5::AST::If;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        return 'if (' . $self->{cond}->emit_xs($level+1) . ") \{\n"
             .  ($self->{body}
                ? join(";\n", 
                       map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{ $self->{body}->stmts } )
                  ) . "\n"
                : ''
                )
             . Perlito5::XS::tab($level) . "}"
             .  ($self->{otherwise} && scalar(@{ $self->{otherwise}->stmts })
                ?  ( "\n"
                    . Perlito5::XS::tab($level) . "else \{\n"
                    .   join( ";\n", 
                              map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{ $self->{otherwise}->stmts } )
                        ) . "\n"
                    . Perlito5::XS::tab($level) . "}"
                    )
                : ''
                );
    }
}

package Perlito5::AST::When;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        return 'when (' . $self->{cond}->emit_xs($level+1) . ") \{\n"
             .  ($self->{body}
                ? join(";\n", 
                       map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{ $self->{body}->stmts } )
                  ) . "\n"
                : ''
                )
             . Perlito5::XS::tab($level) . "}";
    }
}


package Perlito5::AST::While;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $self->{cond};
           'for ( '
        .  ( $self->{init}     ? $self->{init}->emit_xs($level+1)           . '; ' : '; ' )
        .  ( $cond             ? $cond->emit_xs($level+1)                   . '; ' : '; ' )
        .  ( $self->{continue} ? $self->{continue}->emit_xs($level+1)       . ' '  : ' '  )
        .  ') {' . "\n"
        .       join(";\n", 
                     map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{ $self->{body}->stmts } )
                    ) . "\n"
        . Perlito5::XS::tab($level) . "}"
    }
}

package Perlito5::AST::For;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $cond =
               ( $self->{cond}[0] ? $self->{cond}[0]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[1] ? $self->{cond}[1]->emit_javascript($level + 1) . '; '  : '; ' )
            .  ( $self->{cond}[2] ? $self->{cond}[2]->emit_javascript($level + 1) . ' '   : ' '  )
        }
        else {
            $cond = $self->{cond}->emit_xs($level+1)
        }

        my $sig = '';
        my $sig_ast = $self->{body}->sig();
        if (!$sig_ast) {
            # $_
        }
        elsif ($sig_ast->{decl}) {
            $sig = $sig_ast->{decl} . ' ' . $sig_ast->{type} . ' ' . $sig_ast->{var}->emit_xs($level+1) . ' ';
        }
        else {
            $sig = $sig_ast->emit_xs($level+1) . ' ';
        }
        return 'for ' . $sig . '(' . $cond . ') {' . "\n"
             .   join(";\n", map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{ $self->{body}->stmts } )) . "\n"
             . Perlito5::XS::tab($level) . "}"
    }
}

package Perlito5::AST::Decl;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];

    $self->{type} = 'SV *';
        
        my $decl = $self->{decl};
        my $str =
              $self->{type} . $self->{var}->emit_xs($level+1);
        return $str;
    }
}

package Perlito5::AST::Sub;
{
    sub emit_xs {
        my $self = $_[0];
        my $level = $_[1];
        
        my $name = '';
        #$name = $self->{namespace} . "::" . $self->{name} . " "
        $name = $self->{name} . " "
            if $self->{name};

        my $sig = $self->{sig};
        my $i = 0;
          'void ' . $name . "()\n"
        . "PPCODE:\n"
        .   join(";\n", map( Perlito5::XS::tab($level+1) . $_->emit_xs( $level + 1 ), @{$self->{block}} )) . ";\n"
    }
}

1;

=begin

=head1 NAME

Perlito5::XS::Emit - Code generator for Perlito5-in-XS

=head1 SYNOPSIS

    $program->emit_xs()  # generated XS code

=head1 DESCRIPTION

This module generates XS code for the Perlito compiler.

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

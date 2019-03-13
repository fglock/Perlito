use v5;
use Perlito5::AST;
use strict;
use Perlito5::Perl6::TreeGrammar;

package Perlito5::Perl6 {
    sub emit_perl6_block {
        my $block = $_[0];
        return [ 'block', 
                 map { defined($_) && $_->emit_perl6() } @$block
               ];
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
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        # return 0+$s if (0+$s) eq $s;  # XXX - this breaks with 'nan' (perl6 uses 'NaN')
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
        return @out if @out < 2;
        return [ op => 'list:<~>', @out ];
    }
}

package Perlito5::AST::CompUnit;
{
    sub emit_perl6 {
        my $self = $_[0];
        my @body = grep { defined($_) } @{$self->{body}};
        my @out;
        my $pkg = { name => 'main', body => [] };
        for my $stmt (@body) {
            if (ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{code} eq 'package') {
                $Perlito5::PKG_NAME = $stmt->{namespace};
                push @out, [ stmt => [ keyword => 'class'], [ bareword => $pkg->{name} ],
                             [ block => map { $_->emit_perl6() } @{ $pkg->{body} } ]
                           ]
                    if @{ $pkg->{body} };
                $pkg = { name => $stmt->{namespace}, body => [] };
            }
            else {
                push @{ $pkg->{body} }, $stmt;
            }
        }
        push @out, [ stmt => [ keyword => 'class'], [ bareword => $pkg->{name} ],
                     [ block => map { $_->emit_perl6() } @{ $pkg->{body} } ]
                   ]
            if @{ $pkg->{body} };
        return @out;
    }
    sub emit_perl6_program {
        my $comp_units = $_[0];
        my @body = @$comp_units;
        my @out;
        push @out, [ comment => Perlito5::Compiler::do_not_edit("#") ];
        my $pkg = { name => 'main', body => [] };
        for my $stmt (@body) {
            if (ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->{code} eq 'package') {
                $Perlito5::PKG_NAME = $stmt->{namespace};
                if ( @{ $pkg->{body} } ) {
                    if ( $pkg->{name} eq 'main' ) {
                        push @out, map { $_->emit_perl6() } @{ $pkg->{body} }
                    }
                    else {
                        push @out, [ stmt => [ keyword => 'class'], [ bareword => $pkg->{name} ],
                                     [ block => map { $_->emit_perl6() } @{ $pkg->{body} } ]
                                   ]
                    }
                }
                $pkg = { name => $stmt->{namespace}, body => [] };
            }
            else {
                push @{ $pkg->{body} }, $stmt;
            }
        }
        if ( @{ $pkg->{body} } ) {
            if ( $pkg->{name} eq 'main' ) {
                push @out, map { $_->emit_perl6() } @{ $pkg->{body} }
            }
            else {
                push @out, [ stmt => [ keyword => 'class'], [ bareword => $pkg->{name} ],
                             [ block => map { $_->emit_perl6() } @{ $pkg->{body} } ]
                           ]
            }
        }
        return @out;
    }
}

package Perlito5::AST::Int;
{
    sub emit_perl6 {
        my $self  = $_[0];
        [ number => $self->{int} ];
    }
}

package Perlito5::AST::Num;
{
    sub emit_perl6 {
        my $self  = $_[0];
        [ number => $self->{num} ];
    }
}

package Perlito5::AST::Buf;
{
    sub emit_perl6 {
        my $self  = $_[0];
        Perlito5::Perl6::escape_string( $self->{buf} );
    }
}

package Perlito5::AST::Block;
{
    sub emit_perl6 {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{name}) {
            push @out, [ stmt => [ keyword => $self->{name} ], Perlito5::Perl6::emit_perl6_block($self->{stmts}) ];
        }
        else {
            push @out, Perlito5::Perl6::emit_perl6_block($self->{stmts});
        }
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl6::emit_perl6_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Index;
{
    sub emit_perl6_index {
        my $self = $_[0];
        my $index = $self->{index_exp};
        if (  $index->isa('Perlito5::AST::Apply')
           && $index->{code} eq 'prefix:<->'
           )
        {
            my $arg = $index->{arguments}[0];
            if ($arg->isa('Perlito5::AST::Int')) {
                # [*-1]
                return [ op => 'infix:<->', [ bareword => '*' ], $arg->emit_perl6() ];
            }
        }
        return $self->{index_exp}->emit_perl6();
    }

    sub emit_perl6 {
        my $self = $_[0];
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<@>'
           )
        {
            return [ apply => '[', $self->{obj}->emit_perl6(), $self->emit_perl6_index ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
           )
        {
            $self->{obj}{sigil} = '@';
            return [ apply => '[', $self->{obj}->emit_perl6(), $self->emit_perl6_index ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a[0]
            return [ apply => '[', $self->{obj}{arguments}[0]->emit_perl6(), 
                                   $self->emit_perl6_index ];
        }
        return [ op => 'infix:<.>', $self->{obj}->emit_perl6(), 
                 [ op => 'circumfix:<[ ]>', $self->emit_perl6_index ] ];
    }
}

package Perlito5::AST::Lookup;
{
    sub emit_perl6 {
        my $self = $_[0];
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<@>'
           )
        {
            $self->{obj}{sigil} = '%';
            return [ apply => '{', $self->{obj}->emit_perl6(), $self->autoquote($self->{index_exp})->emit_perl6() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && ( $self->{obj}->sigil eq '$' || $self->{obj}->sigil eq '@' )
           )
        {
            $self->{obj}{sigil} = '%';
            return [ apply => '{', $self->{obj}->emit_perl6(), $self->autoquote($self->{index_exp})->emit_perl6() ];
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{0} ==> $a{0}
            return [ apply => '{', $self->{obj}{arguments}[0]->emit_perl6(), 
                                   $self->autoquote($self->{index_exp})->emit_perl6() ];
        }
        return [ op => 'infix:<.>', $self->{obj}->emit_perl6(), 
                 [ op => 'circumfix:<{ }>', $self->autoquote($self->{index_exp})->emit_perl6() ] ];
    }
}

package Perlito5::AST::Var;
{
    sub emit_perl6 {
        my $self = $_[0];

        if ( $self->{sigil} eq '$#' ) {
            my $v = Perlito5::AST::Var->new( %$self, sigil => '@' );
            return [ op => 'infix:<.>', $v->emit_perl6(), [ keyword => 'end' ] ];
        }

        my $str_name = $self->{name};
        $str_name = '\\\\' if $str_name eq '\\';   # escape $\
        $str_name = '\\"' if $str_name eq '"';     # escape $"

        if ($self->{sigil} eq '::') {
            if ($self->{namespace} eq '__PACKAGE__')  { 
                return [ bareword => '$?PACKAGE' ];
            }
            return $self->{namespace};
        }

        # Normalize the sigil
        my $ns = '';
        if ($self->{namespace}) {
            if ($self->{namespace} eq 'main' && substr($self->{name}, 0, 1) eq '^') {
                # don't add the namespace to special variables
                return $self->{sigil} . '{' . $self->{name} . '}'
            }
            else {
                $ns = $self->{namespace} . '::';
            }
        }
        my $bareword = $ns . $str_name;
        my $c = substr($self->{name}, 0, 1);
        if (  ($c ge 'a' && $c le 'z')
           || ($c ge 'A' && $c le 'Z')
           || ($c eq '_')
           ) 
        {
            return '@*ARGS' if $self->{sigil} eq '@' && $bareword eq 'ARGV';
            return $self->{sigil} . $bareword;
        }

        if ($self->{sigil} eq '$') {
            return '"\n"'           if $bareword eq '/';   # XXX
            return '$*PID'          if $bareword eq '$';
            return '$*PROGRAM_NAME' if $bareword eq '0';
            return '$!'             if $bareword eq '@';
            return '$' . ($bareword - 1) if $bareword >= 1;
        }

        my $str = $self->{sigil} . "{'" . $bareword . "'}";
        return $str;
    }
}


package Perlito5::AST::Call;
{
    sub emit_perl6 {
        my $self = $_[0]; 
        my $invocant = $self->{invocant}->emit_perl6();
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return [ op => 'infix:<.>', $invocant, 
                     [ op => 'circumfix:<[ ]>', $self->{arguments}->emit_perl6() ] ];
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return [ op => 'infix:<.>', $invocant, 
                     [ op => 'circumfix:<{ }>', Perlito5::AST::Lookup->autoquote($self->{arguments})->emit_perl6() ] ];
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
                return [ apply => '(', $invocant, map { $_->emit_perl6() } @{$self->{arguments}} ];
            }
            $meth = '';
        }
        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_perl6();
        }
        if ( $meth ) {
            return [ call => $invocant, $meth, map { $_->emit_perl6() } @{$self->{arguments}} ];
        }
        return [ op => 'infix:<.>', $invocant, [ op => 'list:<,>', map { $_->emit_perl6() } @{$self->{arguments}} ] ];
    }
}

package Perlito5::AST::Apply;
{
    my %special_var = (
        chr(15) => '$*VM',  # $^O
    );
    my %op_translate = (
        'list:<.>'      => 'list:<~>',
        'infix:<.=>'    => 'infix:<~=>',
        'infix:<=~>'    => 'infix:<~~>',
        'infix:<!~>'    => 'infix:<!~~>',
        'infix:<cmp>'   => 'infix:<leq>',
        'ternary:<? :>' => 'ternary:<?? !!>',
        'reverse'       => 'flip',
    );

    sub emit_perl6_args {
        my $self = $_[0];
        return () if !$self->{arguments};
        return map { $_->emit_perl6() } @{$self->{arguments}};
    }
    sub emit_perl6 {
        my $self = $_[0];   
        my $code = $self->{code};

        if (ref $code) {
            return [ op => 'infix:<.>', $code->emit_perl6(), $self->emit_perl6_args() ];
        }

        if ($code eq 'list:<=>>')  { 
            return [ op => $code, 
                     $self->{arguments}[0]->emit_perl6(),
                     $self->{arguments}[1]->emit_perl6() ]
        }
        if ($code eq 'nan' && !$self->{namespace})  { 
            return [ keyword => 'NaN' ];
        }
        if ($code eq 'inf' && !$self->{namespace})  { 
            return [ keyword => 'Inf' ];
        }
        if ($code eq '__PACKAGE__' && !$self->{namespace})  { 
            return [ bareword => '$?PACKAGE' ];
        }
        if ($code eq 'prefix:<$#>') {
            return [ op => 'infix:<.>', $self->{arguments}[0]->emit_perl6(), [ keyword => 'end' ] ];
        }
        if ($code eq 'scalar') {
            my $arg = $self->{arguments}[0];
            if ($arg->isa('Perlito5::AST::Var') && $arg->{sigil} eq '@') {
                # @a.elems
                return [ op => 'infix:<.>', $arg->emit_perl6(), [ keyword => 'elems' ] ];
            }
        }
        if ( (  $code eq 'shift' 
             || $code eq 'pop'
             ) 
           && !@{$self->{arguments}}
           )
        {
            # TODO - @ARGV instead of @_ depending on context
            return [ apply => '(', $code, '@_' ];
        }
        if (  $code eq 'readline' 
           && ref($self->{arguments}[0]) eq 'Perlito5::AST::Buf'
           && $self->{arguments}[0]{buf} eq ''
           )
        {
            # TODO - glob or <> depending on context
            return [ apply => '(', [ keyword => 'lines' ], ];
        }
        if ($code eq 'infix:<x>' ) {
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'circumfix:<( )>') {
                # ($v) x $i
                $code = 'infix:<xx>';
            }
        }
        if ( (  $code eq 'print' 
             || $code eq 'say'
             ) 
           && !@{$self->{arguments}}
           )
        {
            return [ keyword => '.' . $code ];
        }
        if ( $code eq 'infix:<..>' ) {
            Perlito5::Perl6::TreeGrammar->refactor_range_operator($self);
            if (ref($self) ne 'Perlito5::AST::Apply') {
                # the node was refactored into something else
                return $self->emit_perl6();
            }
            $code = $self->{code};
        }

        $code = $op_translate{$code} if $op_translate{$code};

        if ( $code eq 'prefix:<$>' ) {
            my $arg = $self->{arguments}->[0];
            return $special_var{$arg->{buf}}
                if $arg->isa('Perlito5::AST::Buf') && exists $special_var{$arg->{buf}};
        }

        if ( $Perlito5::Perl6::PrettyPrinter::op{ $code } ) {
            return [ op => $code, $self->emit_perl6_args() ];
        }
        if ($code eq 'undef') {
            if (@{$self->{arguments}}) {
                die "TODO - undef(expr)";
            }
            else {
                return 'Any';
            }
        }

        my $ns = '';
        if ($self->{namespace}) {
            $ns = $self->{namespace} . '::';
        }
        $code = $ns . $code;

        if ($self->{code} eq 'p5:s') {
            my $modifier = $self->{arguments}->[2]->{buf};
            $modifier = ':' . $modifier if $modifier;
            return 's:P5' . $modifier 
                 .     '!' . $self->{arguments}->[0]->{buf}   # emit_perl6() 
                 .     '!' . $self->{arguments}->[1]->{buf}   # emit_perl6()
                 .     '!';

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
                        $s .= $ast->emit_perl6();  # variable name
                    }
                }
            }
            my $modifier = $self->{arguments}->[1]->{buf};
            $modifier = ':' . $modifier if $modifier;

            return 'm:P5' . $modifier . '!' . $s . '!';
        }
        if ($self->{code} eq 'p5:tr') {
            return 'tr!' . $self->{arguments}->[0]->{buf}   # emit_perl6() 
                 .   '!' . $self->{arguments}->[1]->{buf}   # emit_perl6()
                 .   '!';
        }

        if ($self->{code} eq 'package')    { 
                $Perlito5::PKG_NAME = $self->{namespace};
                return [ stmt => 'class', [ bareword => $self->{namespace} ] ]
        }

        if ($code eq 'map' || $code eq 'grep' || $code eq 'sort') {    
            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                return [ op => 'prefix:<' . $code . '>',
                         [ 'block', map { $_->emit_perl6() } @{$self->{special_arg}{stmts}} ],
                         [ 'op' => 'list:<,>', $self->emit_perl6_args() ],
                       ]
            }
            return [ apply => '(', $code, $self->emit_perl6_args() ];
        }

        if ( $self->{bareword} && !@{$self->{arguments}} ) {
            return [ bareword => $code ];
        }
        if ( $code eq 'eval' ) {
            $code = 'EVAL';
        }
        return [ apply => '(', $code, $self->emit_perl6_args() ];
    }
}

package Perlito5::AST::If;
{
    sub emit_perl6 {
        my $self = $_[0]; 
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{body}->emit_perl6(),
                                      [ stmt => 'if', $self->{cond}->emit_perl6() ] ];
        }
        if ($self->{otherwise} && ref($self->{otherwise}) ne 'Perlito5::AST::Block') {
            return [ stmt_modifier => $self->{otherwise}->emit_perl6(),
                                      [ stmt => 'unless', $self->{cond}->emit_perl6() ] ];
        }
        my @out = ( [ stmt => [ keyword => 'if' ],
                      $self->{cond}->emit_perl6(),
                      Perlito5::Perl6::emit_perl6_block($self->{body}->stmts)
                    ] );
        my $otherwise = $self->{otherwise};

        while ( $otherwise
              && @{ $otherwise->{stmts} } == 1 
              && ref($otherwise->{stmts}[0]) eq 'Perlito5::AST::If'
              && ($otherwise->{stmts}[0]{body} && ref($otherwise->{stmts}[0]{body}) eq 'Perlito5::AST::Block')
              )
        {
            push @out, [ stmt => [ keyword => 'elsif' ],
                         $otherwise->{stmts}[0]{cond}->emit_perl6(),
                         Perlito5::Perl6::emit_perl6_block($otherwise->{stmts}[0]{body}{stmts})
                       ];
            $otherwise = $otherwise->{stmts}[0]{otherwise};
        }

        return @out if !($otherwise && scalar(@{ $otherwise->stmts }));

        push @out, [ stmt => [ keyword => 'else' ],
                     Perlito5::Perl6::emit_perl6_block($otherwise->stmts)
                   ];
        return @out;
    }
}

package Perlito5::AST::When;
{
    sub emit_perl6 {
        my $self = $_[0];
        return [ stmt => [ keyword => 'when' ],
                 $self->{cond}->emit_perl6(),
                 Perlito5::Perl6::emit_perl6_block($self->{body}->stmts)
               ];
    }
}


package Perlito5::AST::While;
{
    sub emit_perl6 {
        my $self = $_[0];

        Perlito5::Perl6::TreeGrammar->refactor_while_glob($self);
        if (ref($self) ne 'Perlito5::AST::While') {
            # the node was refactored into something else
            return $self->emit_perl6();
        }

        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        
        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_perl6(),
                                      [ stmt => [ keyword => 'while' ], $self->{cond}->emit_perl6() ] ];
        }
        push @out, [ stmt => [ keyword => 'while' ],
                     $self->{cond}->emit_perl6(),
                     Perlito5::Perl6::emit_perl6_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl6::emit_perl6_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::For;
{
    sub emit_perl6 {
        my $self = $_[0];
        my @out;
        push @out, [ label => $self->{label} ]
            if $self->{label};        

        if ($self->{body} && ref($self->{body}) ne 'Perlito5::AST::Block') {
            return @out,
                   [ stmt_modifier => $self->{body}->emit_perl6(),
                                      [ stmt => 'for', $self->{cond}->emit_perl6() ] ];
        }

        my $cond;
        my $keyword;
        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            $keyword = 'loop';
            $cond = [ paren_semicolon => '(', 
                      ( $self->{cond}[0] ? $self->{cond}[0]->emit_perl6() : [] ),
                      ( $self->{cond}[1] ? $self->{cond}[1]->emit_perl6() : [] ),
                      ( $self->{cond}[2] ? $self->{cond}[2]->emit_perl6() : [] ),
                    ];
        }
        else {
            $keyword = 'for';
            $cond = $self->{cond}->emit_perl6();
        }

        my @sig;
        my $sig_ast = $self->{topic};
        if (!$sig_ast) {
            # $_
        }
        else {
            $sig_ast = $sig_ast->{var}
                if ref($sig_ast) eq 'Perlito5::AST::Decl';
            @sig = ( '->', $sig_ast->emit_perl6() );
        }
        push @out, [ stmt => [ keyword => $keyword ],
                     $cond,
                     @sig,
                     Perlito5::Perl6::emit_perl6_block($self->{body}->stmts)
                   ];
        if ($self->{continue} && @{ $self->{continue}{stmts} }) {
            push @out, [ stmt => [ keyword => 'continue' ], Perlito5::Perl6::emit_perl6_block($self->{continue}{stmts}) ]
        }
        return @out;
    }
}

package Perlito5::AST::Decl;
{
    sub emit_perl6 {
        my $self = $_[0];
        return [ op => 'prefix:<' . $self->{decl} . '>', 
                 ($self->{type} ? $self->{type} : ()),
                 $self->{var}->emit_perl6()
               ];
    }
}

package Perlito5::AST::Sub;
{
    sub emit_perl6 {
        my $self = $_[0];

        Perlito5::Perl6::TreeGrammar->refactor_sub_arguments($self);

        my @parts;

        if ($self->{args}) {
            # from refactor_sub_arguments

            push @parts, [ paren => '(', 
                            ( map {[ var => $_->emit_perl6(), '?' ]} @{$self->{args}} ),
                            [ var => '*@_' ]
                         ];
        }
        else {

            # TODO - use sig

            # push @parts, [ paren => '(', [ bareword => $self->{sig} ] ]
            #     if defined $self->{sig};

            push @parts, [ paren => '(', [ var => '*@_' ] ];
        }

        push @parts, Perlito5::Perl6::emit_perl6_block($self->{block}{stmts})
            if defined $self->{block};
        return [ op => 'prefix:<sub>', @parts ] if !$self->{name};

        my $is_our = 1; # our is default in perl5
        $is_our = 0 if $self->{decl} eq 'my';

        my $name = $self->{name};
        if ( $Perlito5::PKG_NAME ne $self->{namespace} ) {
            $name = $self->{namespace} . "::" . $name;
            $is_our = 0;
        }

        if ($is_our) {
            return [ stmt => [ keyword => 'our' ], [ keyword => 'sub' ], [ bareword => $name ], @parts ];
        }
        else {
            return [ stmt => [ keyword => 'sub' ], [ bareword => $name ], @parts ];
        }
    }
}

1;

=begin

=head1 NAME

Perlito5::Perl6::Emit - Code generator for Perlito5-in-Perl5

=head1 SYNOPSIS

    $program->emit_perl6()  # generated Perl6 code

=head1 DESCRIPTION

This module generates Perl6 code for the Perlito compiler.

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

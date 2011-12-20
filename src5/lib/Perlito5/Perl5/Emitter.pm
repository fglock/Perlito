use v5;

use Perlito5::AST;

class Perl5 {
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
        for my $i (0 .. $s->chars() - 1) {
            my $c = substr($s, $i, 1);
            if     (($c ge 'a') && ($c le 'z'))
                || (($c ge 'A') && ($c le 'Z'))
                || (($c ge '0') && ($c le '9'))
                || exists( $safe_char{$c} )
            {
                $tmp = $tmp ~ $c;
            }
            else {
                @out->push: "'$tmp'" if $tmp ne '';
                @out->push: "chr({ ord($c) })";
                $tmp = '';
            }
        }
        @out->push: "'$tmp'" if $tmp ne '';
        return @out->join(' . ');
    }

}

class CompUnit {
    sub emit_perl5 {
        $_[0]->emit_perl5_indented(0)
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];

        my @body;
        for (@.body) {
            if defined($_) {
                push @body, $_
            }
        }
          Perl5::tab($level)    ~ "\{\n"
        ~ Perl5::tab($level)    ~ 'package ' ~ $.name ~ ";" ~ "\n"
        ~ Perl5::tab($level + 1)    ~ 'sub new { shift; bless { @_ }, "' ~ $.name ~ '" }'  ~ "\n"
        ~                             (@body.>>emit_perl5_indented( $level + 1 )).join( ";\n" ) ~ "\n"
        ~ Perl5::tab($level)    ~ "}\n"
        ~ "\n"
    }
    sub emit_perl5_program {
        my $comp_units = $_[0];

        my $str = ''
            ~ "use v5;\n"
            ~ "use utf8;\n"
            ~ "use strict;\n"
            ~ "use warnings;\n"
            ~ "no warnings ('redefine', 'once', 'void', 'uninitialized', 'misc', 'recursion');\n"
            ~ "use Perlito5::Perl5::Runtime;\n"
            ~ "use Perlito5::Perl5::Prelude;\n"
            ~ "our \$MATCH = Perlito5::Match->new();\n";
        for my $comp_unit (@($comp_units)) {
            $str ~= $comp_unit->emit_perl5_indented(0)
        }
        $str ~= "1;\n";
        return $str;
    }
}

class Val::Int {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
         Perl5::tab($level) ~ $.int }
}

class Val::Bit {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
         Perl5::tab($level) ~ $.bit }
}

class Val::Num {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
         Perl5::tab($level) ~ $.num }
}

class Val::Buf {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        Perl5::tab($level) ~ Perl5::escape_string($.buf)
    }
}

class Lit::Block {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
          Perl5::tab($level) ~ "sub \{\n"
        ~   @.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Lit::Array {
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

class Lit::Hash {
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

class Index {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $.obj->isa('Var')
           && $.obj->sigil eq '$'
           )
        {
            my $v = Var->new( sigil => '@', twigil => $.obj->twigil, namespace => $.obj->namespace, name => $.obj->name );
            return $v->emit_perl5_indented($level) ~ '->[' ~ $.index_exp->emit_perl5() ~ ']';
        }

        $.obj->emit_perl5_indented($level) ~ '->[' ~ $.index_exp->emit_perl5() ~ ']';
    }
}

class Lookup {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        

        if (  $.obj->isa('Var')
           && $.obj->sigil eq '$'
           && $.obj->name ne '/'  # XXX $/ is the Perl6 match object
           )
        {
            my $v = Var->new( sigil => '%', twigil => $.obj->twigil, namespace => $.obj->namespace, name => $.obj->name );
            return $v->emit_perl5_indented($level) ~ '->{' ~ $.index_exp->emit_perl5() ~ '}';
        }

        $.obj->emit_perl5_indented($level) ~ '->{' ~ $.index_exp->emit_perl5() ~ '}';
    }
}

class Var {
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
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        else {
            if ($.sigil eq '@') && ($.twigil eq '*') && ($.name eq 'ARGS') {
                return Perl5::tab($level) ~ '(\\@ARGV)'
            }
            if $.twigil eq '.' {
                if $.sigil eq '%' {
                    return Perl5::tab($level) ~ '('
                        ~ 'defined $self->{' ~ $.name ~ '} '
                        ~ '? $self->{' ~ $.name ~ '} '
                        ~ ': ($self->{' ~ $.name ~ "} = bless(\{}, 'HASH')))"
                }
                elsif $.sigil eq '@' {
                    return Perl5::tab($level) ~ '('
                        ~ 'defined $self->{' ~ $.name ~ '} '
                        ~ '? $self->{' ~ $.name ~ '} '
                        ~ ': ($self->{' ~ $.name ~ "} ||= bless([], 'ARRAY')))"
                }
                else {
                    return Perl5::tab($level) ~ '$self->{' ~ $.name ~ '}'
                }
            }
            if $.name eq '/' {
                return Perl5::tab($level) ~ $table->{$.sigil} ~ 'MATCH'
            }
        }
        return Perl5::tab($level) ~ $table->{$.sigil} ~ $ns ~ $.name
    }
    method plain_name {
        if $.namespace {
            return $.namespace ~ '::' ~ $.name
        }
        return $.name
    }
}

class Proto {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        Perl5::tab($level) ~ $.name
    }
}

class Call {

    my %method_perl5 = (
        'perl'   => 'Main::perl',
        'id'     => 'Main::id',
        'yaml'   => 'Main::yaml',
        'say'    => 'Main::say',
        'join'   => 'Main::join',
        'split'  => 'Main::split',
        'chars'  => 'Main::chars',
        'isa'    => 'Main::isa',
        'pairs'  => 'Main::pairs',
        'keys'   => 'Main::keys',
        'values' => 'Main::values',
    );

    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $invocant = $.invocant->emit_perl5;
        if $invocant eq 'self' {
            $invocant = '$self';
        }

        if exists( $method_perl5{ $.method } ) {
            if ($.hyper) {
                return Perl5::tab($level)
                    ~ 'bless [ map { '
                        ~ $method_perl5{ $.method } ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' ~ ' } @{( ' ~ $invocant ~ ' )'
                    ~ '} ], "ARRAY"';
            }
            else {
                return Perl5::tab($level)
                    ~ $method_perl5{ $.method } ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
            }
        }
        if $.method eq 'push' {
            return Perl5::tab($level) ~ 'push( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit_perl5).join(', ') ~ ' )'
        }
        if $.method eq 'unshift' {
            return Perl5::tab($level) ~ 'unshift( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit_perl5).join(', ') ~ ' )'
        }
        if $.method eq 'pop' {
            return Perl5::tab($level) ~ 'pop( @{' ~ $invocant ~ '} )'
        }
        if $.method eq 'shift' {
            return Perl5::tab($level) ~ 'shift( @{' ~ $invocant ~ '} )'
        }
        if $.method eq 'elems' {
            return Perl5::tab($level) ~ 'scalar( @{' ~ $invocant ~ '} )'
        }

        if ( $.method eq 'postcircumfix:<[ ]>' ) {
            return Perl5::tab($level) ~ $invocant ~ '->[' ~ @.arguments->emit_perl5() ~ ']'
        }
        if ( $.method eq 'postcircumfix:<{ }>' ) {
            return Perl5::tab($level) ~ $invocant ~ '->{' ~ @.arguments->emit_perl5() ~ '}'
        }

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth = '';
        }

        my $call = '->' ~ $meth ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        if ($.hyper) {
            if !(  $.invocant->isa( 'Apply' )
                && $.invocant->code eq 'prefix:<@>' )
            {
                $invocant = '@{( ' ~ $invocant ~ ' )}';
            }
            return Perl5::tab($level) ~ '[ map { $_' ~ $call ~ ' } ' ~ $invocant ~ ' ]';
        }
        else {
            Perl5::tab($level) ~ $invocant ~ $call;
        }
    }
}

class Apply {

    my %op_prefix_perl5 = (
        say     => 'Main::say',
        print   => 'Main::print',
        map     => 'Main::map',
        grep    => 'Main::grep',
        sort    => 'Main::sort',
        warn    => 'warn',
        Int     => '0+',
        Num     => '0+',
        bool    => '!!',
        'prefix:<~>'    => '"".',
        'prefix:<!>'    => '!',
        'prefix:<?>'    => '!!',
        'prefix:<++>'   => '++',
        'prefix:<-->'   => '--',
    );

    my %op_infix_perl5 = (
        'list:<~>'   => ' . ',
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
        if $apply {
            return $apply->emit_perl5_indented( $level );
        }

        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;

        if $code->isa( 'Str' ) { }
        else {
            return Perl5::tab($level) ~ '(' ~ $.code->emit_perl5() ~ ')->(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        if exists $op_infix_perl5{$code} {
            return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join( $op_infix_perl5{$code} ) ~ ')'
        }
        if exists $op_prefix_perl5{$code} {
            return Perl5::tab($level) ~ $op_prefix_perl5{$code} ~ '('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')'
        }

        if $.code eq 'package'   { return Perl5::tab($level) ~ 'package ' ~ $.namespace }
        if $code eq 'self'       { return Perl5::tab($level) ~ '$self' }
        if $code eq 'Mu'         { return Perl5::tab($level) ~ 'undef()' }

        if $code eq 'make'       { return Perl5::tab($level) ~ '($MATCH->{capture} = ('   ~ (@.arguments.>>emit_perl5).join(', ') ~ '))' }

        if $code eq 'array'      { return Perl5::tab($level) ~ '@{' ~ (@.arguments.>>emit_perl5).join(' ')           ~ '}'   }
        if $code eq 'pop'        { return Perl5::tab($level) ~ 'pop( @{' ~ (@.arguments.>>emit_perl5).join(' ')      ~ '} )' }
        if $code eq 'push'       { return Perl5::tab($level) ~ 'push( @{' ~ (@.arguments[0]).emit_perl5() ~ '}, ' ~ (@.arguments[1]).emit_perl5() ~ ' )' }
        if $code eq 'shift'      { 
            if ( @.arguments ) {
                return Perl5::tab($level) ~ 'shift( @{' ~ (@.arguments.>>emit_perl5).join(' ')    ~ '} )' 
            }
            return 'shift()'
        }
        if $code eq 'unshift'    { return Perl5::tab($level) ~ 'unshift( @{' ~ (@.arguments.>>emit_perl5).join(' ')  ~ '} )' }

        if $code eq 'prefix:<\\>' { 
            # XXX currently a no-op
            return Perl5::tab($level) ~ (@.arguments.>>emit_perl5).join(' ') 
        }
        if $code eq 'prefix:<$>' { return Perl5::tab($level) ~ '${' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }
        if $code eq 'prefix:<@>' { return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join(' ')     ~ ')' }
        if $code eq 'prefix:<%>' { return Perl5::tab($level) ~ '%{' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }

        if $code eq 'postfix:<++>' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')--' }

        if $code eq 'infix:<..>' { return Perl5::tab($level) ~ '(bless ['  ~ (@.arguments.>>emit_perl5).join(' .. ')  ~ "], 'ARRAY')" }

        if $code eq 'ternary:<?? !!>' {
            return Perl5::tab($level)
                ~  '('  ~ @.arguments[0].emit_perl5
                ~ ' ? ' ~ @.arguments[1].emit_perl5
                ~ ' : ' ~ @.arguments[2].emit_perl5
                ~  ')'
        }

        if $code eq 'circumfix:<( )>' {
            return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' {
            return Perl5::tab($level) ~ emit_perl5_bind( @.arguments[0], @.arguments[1] );
        }
        if $code eq 'return' {
            if   @.arguments
              && @.arguments->elems == 1
            {
                # bug in "return do", see http://www->perlmonks->org/?node_id=648681
                return Perl5::tab($level) ~ 'return scalar (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
            }
            return Perl5::tab($level) ~ 'return (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        Perl5::tab($level) ~ $code ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
    }

    sub emit_perl5_bind {
        my $parameters = shift;
        my $arguments = shift;

        if $parameters->isa( 'Call' ) {

            # $a->[3] = 4
            if  (  $parameters->method eq 'postcircumfix:<{ }>'
                || $parameters->method eq 'postcircumfix:<[ ]>'
                )
            {
                return '(' ~ $parameters->emit_perl5() ~ ' = ' ~ $arguments->emit_perl5() ~ ')';
            }

            # $obj->a = 3
            my $a = $parameters;
            return '((' ~ ($a->invocant).emit_perl5() ~ ')->{' ~ $a->method() ~ '} = ' ~ $arguments->emit_perl5() ~ ')';
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
        '(' ~ $parameters->emit_perl5() ~ ' = ' ~ $arguments->emit_perl5() ~ ')';
    }
}

class If {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        return Perl5::tab($level) ~ 'if (' ~ $.cond->emit_perl5() ~ ") \{\n"
             ~  ($.body
                ? $.body->stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
                : ''
                )
             ~ Perl5::tab($level) ~ "}"
             ~  ($.otherwise && $.otherwise->stmts->elems()
                ?  ( "\n"
                    ~ Perl5::tab($level) ~ "else \{\n"
                    ~   $.otherwise->stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
                    ~ Perl5::tab($level) ~ "}"
                    )
                : ''
                );
    }
}

class While {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $.cond;
        if   $cond->isa( 'Var' )
          && $cond->sigil eq '@'
        {
            $cond = Apply->new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
           Perl5::tab($level) ~ 'for ( '
        ~  ( $.init     ? $.init->emit_perl5()           ~ '; ' : '; ' )
        ~  ( $cond      ? $cond->emit_perl5()            ~ '; ' : '; ' )
        ~  ( $.continue ? $.continue->emit_perl5()       ~ ' '  : ' '  )
        ~  ') {' ~ "\n"
        ~       $.body->stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class For {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $cond = $.cond;
        if !( $cond->isa( 'Var' ) && $cond->sigil eq '@' ) {
            $cond = Lit::Array->new( array1 => [$cond] )
        }
        my $sig;
        if $.body->sig() {
            $sig = 'my ' ~ $.body->sig->emit_perl5() ~ ' ';
        }
        return  Perl5::tab($level) ~ 'for ' ~ $sig ~ '( @{' ~ $cond->emit_perl5() ~ '} ) {' ~ "\n"
             ~   $.body->stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
             ~ Perl5::tab($level) ~ "}"
    }
}

class Decl {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $decl = $.decl;
        my $name = $.var->plain_name;
        if $decl eq 'has' {
            return Perl5::tab($level) ~ 'sub ' ~ $name ~ ' { $_[0]->{' ~ $name ~ '} }';
        }
        my $str =
            '(' ~ $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var->emit_perl5();
        if ($.var).sigil eq '%' {
            $str ~= ' = bless {}, \'HASH\')';
        }
        elsif ($.var).sigil eq '@' {
            $str ~= ' = bless [], \'ARRAY\')';
        }
        else {
            $str ~= ')';
        }
        return Perl5::tab($level) ~ $str;
    }
}

class Method {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $sig = $.sig;
        my $invocant = $sig->invocant;
        my $pos = $sig->positional;
        my $str = '';

        my $i = 1;
        for my $field (@$pos) {
            $str ~= Perl5::tab( $level + 1 ) ~ 'my ' ~ $field->emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
            $i = $i + 1;
        }

          Perl5::tab($level) ~ 'sub ' ~ $.name ~ " \{\n"
        ~ Perl5::tab( $level + 1 ) ~ 'my ' ~ $invocant->emit_perl5() ~ ' = $_[0];' ~ "\n"
        ~   $str ~
        ~   (@.block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Sub {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $sig = $.sig;
        my $pos = $sig->positional;
        my $str = '';
        my $i = 0;
        for my $field (@$pos) {
            $str ~= Perl5::tab( $level + 1 ) ~ 'my ' ~ $field->emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
            $i = $i + 1;
        }
          Perl5::tab($level) ~ 'sub ' ~ $.name ~ " \{\n"
        ~ Perl5::tab( $level + 1 ) ~ 'my $List__ = bless \\@_, "ARRAY";' ~ "\n"
        ~   $str
        ~   (@.block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Do {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        my $block = $self->simplify->block;
          Perl5::tab($level) ~ "do \{\n"
        ~   ($block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Use {
    sub emit_perl5 {
        my $self = $_[0];
        $self->emit_perl5_indented(0) 
    }
    sub emit_perl5_indented {
        my $self = $_[0];
        my $level = $_[1];
        
        if     $.mod eq 'v6' 
            || $.mod eq 'strict'
            || $.mod eq 'feature'
        {
            return "\n"
                ~ Perl5::tab($level) ~ "# use $.mod \n"
        }
        Perl5::tab($level) ~ 'use ' ~ $.mod
    }
}

=begin

=head1 NAME

Perlito5::Perl5::Emit - Code generator for Perlito5-in-Perl5

=head1 SYNOPSIS

    $program.emit_perl5()  # generated Perl5 code

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

use v6;

class Perl5 {
    sub tab($level) {
        my $s = '';
        my $count = $level;
        while $count > 0 {
            $s = $s ~ "    ";
            $count = $count - 1;
        }
        return $s;
    }

    sub to_bool ($cond) {
        if $cond.isa( 'Val::Num' ) || $cond.isa( 'Val::Buf' ) || $cond.isa( 'Val::Int' ) 
          || ( $cond.isa( 'Apply' ) &&
                ( ($cond.code eq 'bool') || ($cond.code eq 'True') || ($cond.code eq 'False')
                ) 
             )
        {
            return $cond;
        }
        return Apply.new( code => 'bool', arguments => [ $cond ] );
    }

    sub escape_string($s) {
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for 0 .. $s.chars() - 1 -> $i {
            my $c = substr($s, $i, 1);
            if     (($c ge 'a') && ($c le 'z'))
                || (($c ge 'A') && ($c le 'Z'))
                || (($c ge '0') && ($c le '9'))
                ||  ($c eq '_')
                ||  ($c eq ',')
                ||  ($c eq '.')
                ||  ($c eq ':')
                ||  ($c eq '-')
                ||  ($c eq '+')
                ||  ($c eq '*')
                ||  ($c eq ' ')
            {
                $tmp = $tmp ~ $c;
            }
            else {
                @out.push "'$tmp'" if $tmp ne '';
                @out.push "chr({ ord($c) })";
                $tmp = '';
            }
        }
        @out.push "'$tmp'" if $tmp ne '';
        return @out.join(' . ');
    }

}

class CompUnit {
    has $.name;
    has @.body;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my @body;
        for @.body {
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
    sub emit_perl5_program( $comp_units ) {
        my $str = ''
            ~ "use v5;\n"
            ~ "use utf8;\n"
            ~ "use strict;\n"
            ~ "use warnings;\n"
            ~ "no warnings ('redefine', 'once', 'void', 'uninitialized', 'misc', 'recursion');\n"
            ~ "use Perlito::Perl5::Runtime;\n"
            ~ "use Perlito::Perl5::Prelude;\n"
            ~ "our \$MATCH = Perlito::Match->new();\n";
        for @($comp_units) -> $comp_unit {
            $str = $str ~ $comp_unit.emit_perl5_indented(0)
        }
        $str = $str ~ "1;\n";
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.int }
}

class Val::Bit {
    has $.bit;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.bit }
}

class Val::Num {
    has $.num;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.num }
}

class Val::Buf {
    has $.buf;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { 
        Perl5::tab($level) ~ Perl5::escape_string($.buf) 
    }
}

class Lit::Block {
    has $.sig;
    has @.stmts;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my @body;
        for @.stmts {
            if defined($_) {
                push @body, $_ 
            }
        }
        (@body.>>emit_perl5_indented( $level )).join(";\n") 
    }
}

class Lit::Array {
    has @.array1;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5_indented($level);
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5_indented($level);
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        $.obj.emit_perl5_indented($level) ~ '->[' ~ $.index_exp.emit_perl5() ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        $.obj.emit_perl5_indented($level) ~ '->{' ~ $.index_exp.emit_perl5() ~ '}';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
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
                return Perl5::tab($level) ~ '$self->{' ~ $.name ~ '}' 
            }
            if $.name eq '/' {
                return Perl5::tab($level) ~ $table{$.sigil} ~ 'MATCH' 
            }
        }
        return Perl5::tab($level) ~ $table{$.sigil} ~ $ns ~ $.name 
    }
    method plain_name {
        if $.namespace {
            return $.namespace ~ '::' ~ $.name
        }
        return $.name
    }
}

class Proto {
    has $.name;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        Perl5::tab($level) ~ $.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $invocant = $.invocant.emit_perl5;
        if $invocant eq 'self' {
            $invocant = '$self';
        }

        if     ($.method eq 'values')
            || ($.method eq 'keys')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return Perl5::tab($level) ~ '[' ~ $.method ~ '( %{' ~ $invocant ~ '} )' ~ ']';
            }
        }

        if     ($.method eq 'perl')
            || ($.method eq 'id')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'split')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
            || ($.method eq 'pairs')
        { 
            if ($.hyper) {
                return Perl5::tab($level) ~ 
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' ~ ' } @{( ' ~ $invocant ~ ' )} ]';
            }
            else {
                return Perl5::tab($level) ~ 
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
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

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth = '';  
        }
        
        my $call = '->' ~ $meth ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        if ($.hyper) {
            if !(  $.invocant.isa( 'Apply' )
                && $.invocant.code eq 'prefix:<@>' )
            {
                $invocant = '@{( ' ~ $invocant ~ ' )}';
            }
            return Perl5::tab($level) ~ '[ map { $_' ~ $call ~ ' } ' ~ $invocant ~ ' ]';
        }
        else {
            $invocant ~ $call;
        }
    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;

        if $code.isa( 'Str' ) { }
        else {
            return Perl5::tab($level) ~ '(' ~ $.code.emit_perl5() ~ ')->(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        if $code eq 'self'       { return Perl5::tab($level) ~ '$self' }
        if $code eq 'Mu'         { return Perl5::tab($level) ~ 'undef' }

        if $code eq 'make'       { return Perl5::tab($level) ~ '($MATCH->{capture} = ('   ~ (@.arguments.>>emit_perl5).join(', ') ~ '))' }

        if $code eq 'say'        { return Perl5::tab($level) ~ 'Main::say('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }
        if $code eq 'print'      { return Perl5::tab($level) ~ 'Main::print(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }
        if $code eq 'warn'       { return Perl5::tab($level) ~ 'warn('        ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }

        if $code eq 'array'      { return Perl5::tab($level) ~ '@{' ~ (@.arguments.>>emit_perl5).join(' ')           ~ '}'   }
        if $code eq 'pop'        { return Perl5::tab($level) ~ 'pop( @{' ~ (@.arguments.>>emit_perl5).join(' ')      ~ '} )' }
        if $code eq 'push'       { return Perl5::tab($level) ~ 'push( @{' ~ (@.arguments[0]).emit_perl5() ~ '}, ' ~ (@.arguments[1]).emit_perl5() ~ ' )' }
        if $code eq 'shift'      { return Perl5::tab($level) ~ 'shift( @{' ~ (@.arguments.>>emit_perl5).join(' ')    ~ '} )' }
        if $code eq 'unshift'    { return Perl5::tab($level) ~ 'unshift( @{' ~ (@.arguments.>>emit_perl5).join(' ')  ~ '} )' }

        if $code eq 'Int'        { return Perl5::tab($level) ~ '(0+' ~ (@.arguments[0]).emit_perl5()             ~ ')' }
        if $code eq 'Num'        { return Perl5::tab($level) ~ '(0+' ~ (@.arguments[0]).emit_perl5()             ~ ')' }
        if $code eq 'bool'       { return Perl5::tab($level) ~ 'Main::bool('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }

        if $code eq 'prefix:<~>' { return Perl5::tab($level) ~ '("" . ' ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }
        if $code eq 'prefix:<!>' { return Perl5::tab($level) ~ '!Main::bool(' ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }
        if $code eq 'prefix:<?>' { return Perl5::tab($level) ~ 'Main::bool('  ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }

        if $code eq 'prefix:<$>' { return Perl5::tab($level) ~ '${' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }
        if $code eq 'prefix:<@>' { return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join(' ')     ~ ' || [])' }
        if $code eq 'prefix:<%>' { return Perl5::tab($level) ~ '%{' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }

        if $code eq 'postfix:<++>' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')--' }
        if $code eq 'prefix:<++>'  { return Perl5::tab($level) ~ '++(' ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')' }
        if $code eq 'prefix:<-->'  { return Perl5::tab($level) ~ '--(' ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')' }

        if $code eq 'list:<~>'   { return Perl5::tab($level) ~ ''   ~ (@.arguments.>>emit_perl5).join(' . ')   ~ ''  }
        if $code eq 'infix:<+>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' + ')   ~ ')' }
        if $code eq 'infix:<->'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' - ')   ~ ')' }
        if $code eq 'infix:<*>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' * ')   ~ ')' }
        if $code eq 'infix:</>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' / ')   ~ ')' }
        if $code eq 'infix:<>>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' > ')   ~ ')' }
        if $code eq 'infix:<<>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' < ')   ~ ')' }
        if $code eq 'infix:<>=>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' >= ')  ~ ')' }
        if $code eq 'infix:<<=>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' <= ')  ~ ')' }
        if $code eq 'infix:<x>'  { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' x ')   ~ ')' }
        
        if $code eq 'infix:<&&>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' && ')  ~ ')' }
        if $code eq 'infix:<||>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' || ')  ~ ')' }
        if $code eq 'infix:<and>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' and ')  ~ ')' }
        if $code eq 'infix:<or>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' or ')  ~ ')' }
        if $code eq 'infix:<//>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' // ')  ~ ')' }
        if $code eq 'infix:<eq>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' eq ')  ~ ')' }
        if $code eq 'infix:<ne>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' ne ')  ~ ')' }
        if $code eq 'infix:<le>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' le ')  ~ ')' }
        if $code eq 'infix:<ge>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' ge ')  ~ ')' }
 
        if $code eq 'infix:<==>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' == ')  ~ ')' }
        if $code eq 'infix:<!=>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' != ')  ~ ')' }
        if $code eq 'infix:<=>>' { return Perl5::tab($level) ~ '('  ~ (@.arguments.>>emit_perl5).join(' => ')  ~ ')' }
        if $code eq 'infix:<..>' { return Perl5::tab($level) ~ '['  ~ (@.arguments.>>emit_perl5).join(' .. ')  ~ ']' }
        if $code eq 'infix:<===>' { 
            return Perl5::tab($level) ~ '(Main::id(' ~ (@.arguments[0]).emit_perl5() ~ ') eq Main::id(' ~ (@.arguments[1]).emit_perl5() ~ '))' 
        }

        if $code eq 'ternary:<?? !!>' { 
            my $cond = @.arguments[0];
            if   $cond.isa( 'Var' ) 
              && $cond.sigil eq '@' 
            {
                $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
            }
            return Perl5::tab($level) ~ '(' ~ Perl5::to_bool($cond).emit_perl5() ~
                 ' ? ' ~ (@.arguments[1]).emit_perl5() ~
                 ' : ' ~ (@.arguments[2]).emit_perl5() ~
                  ')' 
        }
        
        if $code eq 'circumfix:<( )>' {
            return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' { 
            return Perl5::tab($level) ~ emit_perl5_bind( @.arguments[0], @.arguments[1] );
        }
        if $code eq 'return' {
            if   @.arguments 
              && @.arguments.elems == 1
            {
                # bug in "return do", see http://www.perlmonks.org/?node_id=648681
                return Perl5::tab($level) ~ 'return scalar (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
            }
            return Perl5::tab($level) ~ 'return (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        Perl5::tab($level) ~ $code ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
    }

    sub emit_perl5_bind ($parameters, $arguments) {
        if $parameters.isa( 'Call' ) {

            # $obj.a = 3

            my $a = $parameters;
            return '((' ~ ($a.invocant).emit_perl5() ~ ')->{' ~ $a.method() ~ '} = ' ~ $arguments.emit_perl5() ~ ')';
        }

        if $parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $parameters.array1;
            my $str = 'do { ';
            my $i = 0;
            for @$a -> $var { 
                $str = $str ~ ' ' 
                    ~ emit_perl5_bind( $var, 
                            Index.new(
                                obj    => $arguments,
                                index_exp  => Val::Int.new( int => $i )
                            )
                        ) 
                    ~ '; ';
                $i = $i + 1;
            }
            return $str ~ $parameters.emit_perl5() ~ ' }';
        }
        if $parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} = { a => 1, b => [2, 3]}

            my $a = $parameters.hash1;
            my $b = $arguments.hash1;
            my $str = 'do { ';
            my $i = 0;
            my $arg;
            for @$a -> $var {
                $arg = Apply.new(code => 'Mu', arguments => []);
                for @$b -> $var2 {
                    if ($var2[0]).buf eq ($var[0]).buf() {
                        $arg = $var2[1];
                    }
                }
                $str = $str ~ ' ' ~ emit_perl5_bind( $var[1], $arg ) ~ '; ';
                $i = $i + 1;
            }
            return $str ~ $parameters.emit_perl5() ~ ' }';
        }
        if      $parameters.isa( 'Var' ) && $parameters.sigil eq '@' 
            ||  $parameters.isa( 'Decl' ) && $parameters.var.sigil eq '@' 
        {
            $arguments = Lit::Array.new( array1 => [$arguments] );
        }
        elsif   $parameters.isa( 'Var' ) && $parameters.sigil eq '%' 
            ||  $parameters.isa( 'Decl' ) && $parameters.var.sigil eq '%' 
        {
            $arguments = Lit::Hash.new( hash1 => [$arguments] );
        }
        '(' ~ $parameters.emit_perl5() ~ ' = ' ~ $arguments.emit_perl5() ~ ')';
    }
}

class If {
    has $.cond;
    has $.body;
    has $.otherwise;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        return Perl5::tab($level) ~ 'if (' ~ Perl5::to_bool($.cond).emit_perl5() ~ ") \{\n" 
             ~   (($.body).emit_perl5_indented( $level + 1 )) ~ "\n"
             ~ Perl5::tab($level) ~ "}" 
             ~  ($.otherwise 
                ??  ( "\n"
                    ~ Perl5::tab($level) ~ "else \{\n" 
                    ~   (($.otherwise).emit_perl5_indented( $level + 1 )) ~ "\n" 
                    ~ Perl5::tab($level) ~ "}"
                    )
                !! '' 
                );
    }
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has $.body;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
           Perl5::tab($level) ~ 'for ( '
        ~  ( $.init     ?? $.init.emit_perl5()           ~ '; ' !! '; ' )
        ~  ( $cond      ?? Perl5::to_bool($cond).emit_perl5() ~ '; ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_perl5()       ~ ' '  !! ' '  )
        ~  ') {' ~ "\n"
        ~       $.body.emit_perl5_indented( $level + 1 ) ~ "\n"
        ~ Perl5::tab($level) ~ "}" 
    }
}

class For {
    has $.cond;
    has $.body;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $cond = $.cond;
        if !( $cond.isa( 'Var' ) && $cond.sigil eq '@' ) {
            $cond = Lit::Array.new( array1 => [$cond] )
        }
        my $sig;
        if $.body.sig() {
            $sig = 'my ' ~ $.body.sig.emit_perl5() ~ ' ';
        }
        return  Perl5::tab($level) ~ 'for ' ~ $sig ~ '( @{' ~ $cond.emit_perl5() ~ ' || []} ) {' ~ "\n" 
             ~   $.body.emit_perl5_indented( $level + 1 ) ~ "\n"
             ~ Perl5::tab($level) ~ "}" 
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.plain_name;
        if $decl eq 'has' {
            return Perl5::tab($level) ~ 'sub ' ~ $name ~ ' { $_[0]->{' ~ $name ~ '} }';
        }
        my $str = 
            '(' ~ $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_perl5() ~ ' = ';
        if ($.var).sigil eq '%' {
            $str = $str ~ '{})';
        }
        elsif ($.var).sigil eq '@' {
            $str = $str ~ '[])';
        }
        else {
            $str = $str ~ 'undef)';
        }
        return Perl5::tab($level) ~ $str;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = '';

        my $i = 1;
        for @$pos -> $field { 
            $str = $str ~ Perl5::tab( $level + 1 ) ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
            $i = $i + 1;
        }

          Perl5::tab($level) ~ 'sub ' ~ $.name ~ " \{\n"
        ~ Perl5::tab( $level + 1 ) ~ 'my ' ~ $invocant.emit_perl5() ~ ' = $_[0];' ~ "\n"
        ~   $str ~
        ~   (@.block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n" 
        ~ Perl5::tab($level) ~ "}"
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = '';
        my $i = 0;
        for @$pos -> $field { 
            $str = $str ~ Perl5::tab( $level + 1 ) ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
            $i = $i + 1;
        }
          Perl5::tab($level) ~ 'sub ' ~ $.name ~ " \{\n" 
        ~   $str 
        ~   (@.block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n" 
        ~ Perl5::tab($level) ~ "}"
    }
}

class Do {
    has $.block;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $block = self.simplify.block;
          Perl5::tab($level) ~ "do \{\n" 
        ~   ($block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Use {
    has $.mod;
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        if $.mod eq 'v6' {
            return "\n"
                ~ Perl5::tab($level) ~ "# use $.mod \n"
        } 
        Perl5::tab($level) ~ 'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

Perlito::Perl5::Emit - Code generator for Perlito-in-Perl5

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

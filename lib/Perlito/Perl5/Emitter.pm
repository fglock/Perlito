use v6;

class Perl5 {
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
}

class CompUnit {
    has $.name;
    has @.body;
    method emit_perl5 {
        my @body;
        for @.body {
            if defined($_) {
                push @body, $_ 
            }
        }
          "\{\n"
        ~ 'package ' ~ $.name ~ ";" ~ "\n" 
        ~ 'sub new { shift; bless { @_ }, "' ~ $.name ~ '" }'  ~ "\n" 
        ~ (@body.>>emit_perl5).join( ";" ~ "\n" ) ~ "\n"
        ~ "}\n"
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
            $str = $str ~ $comp_unit.emit_perl5
        }
        $str = $str ~ "1;\n";
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit_perl5 { $.int }
}

class Val::Bit {
    has $.bit;
    method emit_perl5 { $.bit }
}

class Val::Num {
    has $.num;
    method emit_perl5 { $.num }
}

class Val::Buf {
    has $.buf;
    method emit_perl5 { '\'' ~ Main::perl_escape_string($.buf) ~ '\'' }
}

class Lit::Block {
    has $.sig;
    has @.stmts;
    method emit_perl5 {
        my @body;
        for @.stmts {
            if defined($_) {
                push @body, $_ 
            }
        }
        (@body.>>emit_perl5).join('; ') 
    }
}

class Lit::Array {
    has @.array1;
    method emit_perl5 {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5;
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_perl5 {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5;
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_perl5 {
        $.obj.emit_perl5() ~ '->[' ~ $.index_exp.emit_perl5() ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_perl5 {
        $.obj.emit_perl5() ~ '->{' ~ $.index_exp.emit_perl5() ~ '}';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_perl5 {
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
                return '(\\@ARGV)'
            }
            if $.twigil eq '.' {
                return '$self->{' ~ $.name ~ '}' 
            }
            if $.name eq '/' {
                return $table{$.sigil} ~ 'MATCH' 
            }
        }
        return $table{$.sigil} ~ $ns ~ $.name 
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
    method emit_perl5 {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_perl5 {
        my $invocant = $.invocant.emit_perl5;
        if $invocant eq 'self' {
            $invocant = '$self';
        }

        if     ($.method eq 'shift')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return 'shift( @{' ~ $invocant ~ '} )';
            }
        }

        if     ($.method eq 'values')
            || ($.method eq 'keys')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return '[' ~ $.method ~ '( %{' ~ $invocant ~ '} )' ~ ']';
            }
        }

        if     ($.method eq 'perl')
            || ($.method eq 'id')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
            || ($.method eq 'pairs')
        { 
            if ($.hyper) {
                return 
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
            }
        }
        if $.method eq 'push' { 
            return 'push( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit_perl5).join(', ') ~ ' )' 
        }
        if $.method eq 'unshift' { 
            return 'unshift( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit_perl5).join(', ') ~ ' )' 
        }
        if $.method eq 'pop' { 
            return 'pop( @{' ~ $invocant ~ '} )' 
        }
        if $.method eq 'shift' { 
            return 'shift( @{' ~ $invocant ~ '} )' 
        }
        if $.method eq 'elems' { 
            return 'scalar( @{' ~ $invocant ~ '} )' 
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
                $invocant = '@{ ' ~ $invocant ~ ' }';
            }
            return '[ map { $_' ~ $call ~ ' } ' ~ $invocant ~ ' ]';
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
    method emit_perl5 {
        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_perl5() ~ ')->(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        if $code eq 'self'       { return '$self' }
        if $code eq 'Mu'         { return 'undef' }

        if $code eq 'make'       { return '($MATCH->{capture} = ('   ~ (@.arguments.>>emit_perl5).join(', ') ~ '))' }

        if $code eq 'say'        { return 'Main::say('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }
        if $code eq 'print'      { return 'Main::print(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }
        if $code eq 'warn'       { return 'warn('        ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }

        if $code eq 'array'      { return '@{' ~ (@.arguments.>>emit_perl5).join(' ')           ~ '}'   }
        if $code eq 'pop'        { return 'pop( @{' ~ (@.arguments.>>emit_perl5).join(' ')      ~ '} )' }
        if $code eq 'push'       { return 'push( @{' ~ (@.arguments[0]).emit_perl5() ~ '}, ' ~ (@.arguments[1]).emit_perl5() ~ ' )' }
        if $code eq 'shift'      { return 'shift( @{' ~ (@.arguments.>>emit_perl5).join(' ')    ~ '} )' }
        if $code eq 'unshift'    { return 'unshift( @{' ~ (@.arguments.>>emit_perl5).join(' ')  ~ '} )' }

        if $code eq 'Int'        { return '(0+' ~ (@.arguments[0]).emit_perl5()             ~ ')' }
        if $code eq 'Num'        { return '(0+' ~ (@.arguments[0]).emit_perl5()             ~ ')' }
        if $code eq 'bool'       { return 'Main::bool('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' }

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }
        if $code eq 'prefix:<!>' { return '!Main::bool(' ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }
        if $code eq 'prefix:<?>' { return 'Main::bool('  ~ (@.arguments.>>emit_perl5).join(' ') ~ ')' }

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_perl5).join(' ')     ~ ' || [])' }
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }

        if $code eq 'postfix:<++>' { return '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')--' }
        if $code eq 'prefix:<++>'  { return '++(' ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')' }
        if $code eq 'prefix:<-->'  { return '--(' ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')' }

        if $code eq 'list:<~>'   { return ''   ~ (@.arguments.>>emit_perl5).join(' . ')   ~ ''  }
        if $code eq 'infix:<+>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' + ')   ~ ')' }
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_perl5).join(' - ')   ~ ')' }
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' * ')   ~ ')' }
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' / ')   ~ ')' }
        if $code eq 'infix:<>>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' > ')   ~ ')' }
        if $code eq 'infix:<<>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' < ')   ~ ')' }
        if $code eq 'infix:<>=>' { return '('  ~ (@.arguments.>>emit_perl5).join(' >= ')  ~ ')' }
        if $code eq 'infix:<<=>' { return '('  ~ (@.arguments.>>emit_perl5).join(' <= ')  ~ ')' }
        if $code eq 'infix:<x>'  { return '('  ~ (@.arguments.>>emit_perl5).join(' x ')   ~ ')' }
        
        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit_perl5).join(' && ')  ~ ')' }
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit_perl5).join(' || ')  ~ ')' }
        if $code eq 'infix:<and>' { return '('  ~ (@.arguments.>>emit_perl5).join(' and ')  ~ ')' }
        if $code eq 'infix:<or>' { return '('  ~ (@.arguments.>>emit_perl5).join(' or ')  ~ ')' }
        if $code eq 'infix:<//>' { return '('  ~ (@.arguments.>>emit_perl5).join(' // ')  ~ ')' }
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit_perl5).join(' eq ')  ~ ')' }
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit_perl5).join(' ne ')  ~ ')' }
        if $code eq 'infix:<le>' { return '('  ~ (@.arguments.>>emit_perl5).join(' le ')  ~ ')' }
        if $code eq 'infix:<ge>' { return '('  ~ (@.arguments.>>emit_perl5).join(' ge ')  ~ ')' }
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit_perl5).join(' == ')  ~ ')' }
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit_perl5).join(' != ')  ~ ')' }
        if $code eq 'infix:<=>>' { return '('  ~ (@.arguments.>>emit_perl5).join(' => ')  ~ ')' }
        if $code eq 'infix:<..>' { return '['  ~ (@.arguments.>>emit_perl5).join(' .. ')  ~ ']' }
        if $code eq 'infix:<===>' { 
            return '(Main::id(' ~ (@.arguments[0]).emit_perl5() ~ ') eq Main::id(' ~ (@.arguments[1]).emit_perl5() ~ '))' 
        }

        if $code eq 'ternary:<?? !!>' { 
            my $cond = @.arguments[0];
            if   $cond.isa( 'Var' ) 
              && $cond.sigil eq '@' 
            {
                $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
            }
            return '(' ~ Perl5::to_bool($cond).emit_perl5() ~
                 ' ? ' ~ (@.arguments[1]).emit_perl5() ~
                 ' : ' ~ (@.arguments[2]).emit_perl5() ~
                  ')' 
        }
        
        if $code eq 'circumfix:<( )>' {
            return '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' { 
            return emit_perl5_bind( @.arguments[0], @.arguments[1] );
        }
        if $code eq 'return' {
            if   @.arguments 
              && @.arguments.elems == 1
            {
                # bug in "return do", see http://www.perlmonks.org/?node_id=648681
                return 'return scalar (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
            }
            return 'return (' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        $code ~ '(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
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
    method emit_perl5 {
        return 'if (' ~ Perl5::to_bool($.cond).emit_perl5() ~ ') { ' 
             ~   (($.body).emit_perl5) 
             ~ ' } ' 
             ~  ($.otherwise 
                ??  ( 'else { ' 
                    ~   (($.otherwise).emit_perl5) 
                    ~ ' }'
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
    method emit_perl5 {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
           'for ( '
        ~  ( $.init     ?? $.init.emit_perl5()           ~ '; ' !! '; ' )
        ~  ( $cond      ?? Perl5::to_bool($cond).emit_perl5() ~ '; ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_perl5()       ~ ' '  !! ' '  )
        ~  ') { ' 
        ~       $.body.emit_perl5() 
        ~ ' }'
    }
}

class For {
    has $.cond;
    has $.body;
    method emit_perl5 {
        my $cond = $.cond;
        if !( $cond.isa( 'Var' ) && $cond.sigil eq '@' ) {
            $cond = Lit::Array.new( array1 => [$cond] )
        }
        my $sig;
        if $.body.sig() {
            $sig = 'my ' ~ $.body.sig.emit_perl5() ~ ' ';
        }
        return  'for ' ~ $sig ~ '( @{' ~ $cond.emit_perl5() ~ ' || []} ) { ' 
             ~   $.body.emit_perl5() 
             ~ ' }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_perl5 {
        my $decl = $.decl;
        my $name = $.var.plain_name;
        if $decl eq 'has' {
            return 'sub ' ~ $name ~ ' { $_[0]->{' ~ $name ~ '} }';
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
        return $str;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_perl5 {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_perl5 {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = '';

        my $i = 1;
        for @$pos -> $field { 
            $str = $str ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ ']; ';
            $i = $i + 1;
        }

        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.emit_perl5() ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit_perl5).join('; ') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_perl5 {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = '';
        my $i = 0;
        for @$pos -> $field { 
            $str = $str ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ ']; ';
            $i = $i + 1;
        }
        'sub ' ~ $.name ~ ' { ' ~ 
          $str ~
          (@.block.>>emit_perl5).join('; ') ~ 
        ' }'
    }
}

class Do {
    has $.block;
    method emit_perl5 {
        'do { ' ~ ($.block.emit_perl5) ~ ' }'
    }
}

class Use {
    has $.mod;
    method emit_perl5 {
        if $.mod eq 'v6' {
            return "\n# use $.mod \n"
        } 
        'use ' ~ $.mod
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

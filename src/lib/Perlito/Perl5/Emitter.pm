use v6;

use Perlito::AST;

class Perl5 {
    sub tab($level) {
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

    sub escape_string($s) {
        my @out;
        my $tmp = '';
        return "''" if $s eq '';
        for 0 .. $s.chars() - 1 -> $i {
            my $c = substr($s, $i, 1);
            if     (($c ge 'a') && ($c le 'z'))
                || (($c ge 'A') && ($c le 'Z'))
                || (($c ge '0') && ($c le '9'))
                || exists( %safe_char{$c} )
            {
                $tmp = $tmp ~ $c;
            }
            else {
                @out.push: "'$tmp'" if $tmp ne '';
                @out.push: "chr({ ord($c) })";
                $tmp = '';
            }
        }
        @out.push: "'$tmp'" if $tmp ne '';
        return @out.join(' . ');
    }

}

class CompUnit {
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
            $str ~= $comp_unit.emit_perl5_indented(0)
        }
        $str ~= "1;\n";
        return $str;
    }
}

class Val::Int {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.int }
}

class Val::Bit {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.bit }
}

class Val::Num {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { Perl5::tab($level) ~ $.num }
}

class Val::Buf {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) { 
        Perl5::tab($level) ~ Perl5::escape_string($.buf) 
    }
}

class Lit::Block {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
          Perl5::tab($level) ~ "sub \{\n" 
        ~   @.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Lit::Array {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5_indented($level);
    }
}

class Lit::Hash {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_perl5_indented($level);
    }
}

class Index {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        $.obj.emit_perl5_indented($level) ~ '->[' ~ $.index_exp.emit_perl5() ~ ']';
    }
}

class Lookup {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        $.obj.emit_perl5_indented($level) ~ '->{' ~ $.index_exp.emit_perl5() ~ '}';
    }
}

class Var {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
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
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
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

    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $invocant = $.invocant.emit_perl5;
        if $invocant eq 'self' {
            $invocant = '$self';
        }

        if exists( %method_perl5{ $.method } ) {
            if ($.hyper) {
                return Perl5::tab($level)  
                    ~ 'bless [ map { ' 
                        ~ %method_perl5{ $.method } ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')' ~ ' } @{( ' ~ $invocant ~ ' )' 
                    ~ '} ], "ARRAY"';
            }
            else {
                return Perl5::tab($level) 
                    ~ %method_perl5{ $.method } ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
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

    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {

        my $apply = self.op_assign();
        if $apply {
            return $apply.emit_perl5_indented( $level );
        }

        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;

        if $code.isa( 'Str' ) { }
        else {
            return Perl5::tab($level) ~ '(' ~ $.code.emit_perl5() ~ ')->(' ~ (@.arguments.>>emit_perl5).join(', ') ~ ')';
        }

        if exists %op_infix_perl5{$code} {
            return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join( %op_infix_perl5{$code} ) ~ ')'
        }
        if exists %op_prefix_perl5{$code} {
            return Perl5::tab($level) ~ %op_prefix_perl5{$code} ~ '('   ~ (@.arguments.>>emit_perl5).join(', ') ~ ')'
        }

        if $code eq 'self'       { return Perl5::tab($level) ~ '$self' }
        if $code eq 'Mu'         { return Perl5::tab($level) ~ 'undef()' }

        if $code eq 'make'       { return Perl5::tab($level) ~ '($MATCH->{capture} = ('   ~ (@.arguments.>>emit_perl5).join(', ') ~ '))' }

        if $code eq 'array'      { return Perl5::tab($level) ~ '@{' ~ (@.arguments.>>emit_perl5).join(' ')           ~ '}'   }
        if $code eq 'pop'        { return Perl5::tab($level) ~ 'pop( @{' ~ (@.arguments.>>emit_perl5).join(' ')      ~ '} )' }
        if $code eq 'push'       { return Perl5::tab($level) ~ 'push( @{' ~ (@.arguments[0]).emit_perl5() ~ '}, ' ~ (@.arguments[1]).emit_perl5() ~ ' )' }
        if $code eq 'shift'      { return Perl5::tab($level) ~ 'shift( @{' ~ (@.arguments.>>emit_perl5).join(' ')    ~ '} )' }
        if $code eq 'unshift'    { return Perl5::tab($level) ~ 'unshift( @{' ~ (@.arguments.>>emit_perl5).join(' ')  ~ '} )' }

        if $code eq 'prefix:<$>' { return Perl5::tab($level) ~ '${' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }
        if $code eq 'prefix:<@>' { return Perl5::tab($level) ~ '(' ~ (@.arguments.>>emit_perl5).join(' ')     ~ ')' }
        if $code eq 'prefix:<%>' { return Perl5::tab($level) ~ '%{' ~ (@.arguments.>>emit_perl5).join(' ')     ~ '}' }

        if $code eq 'postfix:<++>' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return Perl5::tab($level) ~ '('   ~ (@.arguments.>>emit_perl5).join(' ')  ~ ')--' }

        if $code eq 'infix:<..>' { return Perl5::tab($level) ~ '(bless ['  ~ (@.arguments.>>emit_perl5).join(' .. ')  ~ "], 'ARRAY')" }
        if $code eq 'infix:<===>' { 
            return Perl5::tab($level) ~ '(Main::id(' ~ (@.arguments[0]).emit_perl5() ~ ') eq Main::id(' ~ (@.arguments[1]).emit_perl5() ~ '))' 
        }

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
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        return Perl5::tab($level) ~ 'if (' ~ $.cond.emit_perl5() ~ ") \{\n" 
             ~  ($.body 
                ?? $.body.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
                !! ''
                )
             ~ Perl5::tab($level) ~ "}" 
             ~  ($.otherwise && $.otherwise.stmts.elems()
                ??  ( "\n"
                    ~ Perl5::tab($level) ~ "else \{\n" 
                    ~   $.otherwise.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
                    ~ Perl5::tab($level) ~ "}"
                    )
                !! '' 
                );
    }
}

class While {
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
        ~  ( $cond      ?? $cond.emit_perl5()            ~ '; ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_perl5()       ~ ' '  !! ' '  )
        ~  ') {' ~ "\n"
        ~       $.body.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}" 
    }
}

class For {
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
        return  Perl5::tab($level) ~ 'for ' ~ $sig ~ '( @{' ~ $cond.emit_perl5() ~ '} ) {' ~ "\n" 
             ~   $.body.stmts.>>emit_perl5_indented( $level + 1 ).join(";\n") ~ "\n"
             ~ Perl5::tab($level) ~ "}" 
    }
}

class Decl {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.plain_name;
        if $decl eq 'has' {
            return Perl5::tab($level) ~ 'sub ' ~ $name ~ ' { $_[0]->{' ~ $name ~ '} }';
        }
        my $str = 
            '(' ~ $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_perl5();
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
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = '';

        my $i = 1;
        for @$pos -> $field { 
            $str ~= Perl5::tab( $level + 1 ) ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
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
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = '';
        my $i = 0;
        for @$pos -> $field { 
            $str ~= Perl5::tab( $level + 1 ) ~ 'my ' ~ $field.emit_perl5() ~ ' = $_[' ~ $i ~ '];' ~ "\n";
            $i = $i + 1;
        }
          Perl5::tab($level) ~ 'sub ' ~ $.name ~ " \{\n" 
        ~   $str 
        ~   (@.block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n" 
        ~ Perl5::tab($level) ~ "}"
    }
}

class Do {
    method emit_perl5 { self.emit_perl5_indented(0) }
    method emit_perl5_indented( $level ) {
        my $block = self.simplify.block;
          Perl5::tab($level) ~ "do \{\n" 
        ~   ($block.>>emit_perl5_indented( $level + 1 )).join(";\n") ~ "\n"
        ~ Perl5::tab($level) ~ "}"
    }
}

class Use {
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

use v6;

class Perl5 {
    # sub to_bool ($cond) {
    #     if ( $cond.isa( 'Var' ) && $cond.sigil eq '@' )
    #       || $cond.isa( 'Lit::Array' )
    #     {
    #         return Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
    #     }
    #     if $cond.isa( 'Val::Num' ) || $cond.isa( 'Val::Buf' ) || $cond.isa( 'Val::Int' ) 
    #       || $cond.isa( 'Val::Undef' )
    #       || ( $cond.isa( 'Apply' ) &&
    #             ( ($cond.code eq 'bool') || ($cond.code eq 'True') || ($cond.code eq 'False')
    #             ) 
    #          )
    #     {
    #         return $cond;
    #     }
    #     return Apply.new( code => 'bool', arguments => [ $cond ] );
    # }

    sub to_bool ($op, $args) {
        my @s;
        for @($args) -> $cond {
            if     ($cond.isa( 'Val::Int' ))
                || ($cond.isa( 'Val::Num' ))
            {
                push @s, '(' ~ $cond.emit ~ ' != 0 )';
            }
            elsif  (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<||>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<&&>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<!>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<?>'))
                || ($cond.isa( 'Val::Bit' ))
            {
                push @s, $cond.emit;
            }
            else {
                push @s, 'Main::bool(' ~ $cond.emit ~ ')';
            }
        }
        return '(' ~ @s.join($op) ~ ')'
    }

}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {
          "{\n"
        ~ 'package ' ~ $.name ~ ";" ~ "\n" 
        ~ 'sub new { shift; bless { @_ }, "' ~ $.name ~ '" }'  ~ "\n" 
        ~ (@.body.>>emit).join( ";" ~ "\n" ) ~ "\n"
        ~ "}\n"
        ~ "\n"
    }
    sub emit_perl5_program( $comp_units ) {
        my $str = '';
        for @($comp_units) -> $comp_unit {
            $str = $str ~ $comp_unit.emit
        }
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit { $.int }
}

class Val::Bit {
    has $.bit;
    method emit { $.bit }
}

class Val::Num {
    has $.num;
    method emit { $.num }
}

class Val::Buf {
    has $.buf;
    method emit { '\'' ~ Main::perl_escape_string($.buf) ~ '\'' }
}

class Val::Undef {
    method emit { '(undef)' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit {
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit {
        my @s;
        for @.array1 -> $item {
            if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
            {
                push @s, '@{' ~ $item.emit ~ '}';
            }
            else {
                push @s, $item.emit;
            }
        }
        '[' ~ @s.join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash1;
    method emit {
        my $fields = @.hash1;
        my $str = '';
        for @$fields -> $field { 
            $str = $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    # XXX
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit {
        # $.class ~ '->new( ' ~ @.fields.>>emit.join(', ') ~ ' )';
        my $fields = @.fields;
        my $str = '';
        # say @fields.map(sub { $_[0].emit ~ ' => ' ~ $_[1].emit}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str = $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        $.class ~ '->new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit {
        $.obj.emit ~ '->[' ~ $.index_exp.emit ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit {
        $.obj.emit ~ '->{' ~ $.index_exp.emit ~ '}';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit {
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
        };
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
    };
    method plain_name {
        if $.namespace {
            return $.namespace ~ '::' ~ $.name
        }
        return $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        if $.parameters.isa( 'Call' ) {

            # $obj.a = 3

            my $a = $.parameters;
            return '((' ~ ($a.invocant).emit ~ ')->{' ~ $a.method ~ '} = ' ~ $.arguments.emit ~ ')';
        }

        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $.parameters.array1;
            #my $b = $.arguments.array1;
            my $str = 'do { ';
            my $i = 0;
            for @$a -> $var { 
                my $bind = Bind.new( 
                    parameters => $var, 
                    # arguments => ($b[$i]) );
                    arguments  => Index.new(
                        obj    => $.arguments,
                        index_exp  => Val::Int.new( int => $i )
                    )
                );
                $str = $str ~ ' ' ~ $bind.emit ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} = { a => 1, b => [2, 3]}

            my $a = $.parameters.hash1;
            my $b = $.arguments.hash1;
            my $str = 'do { ';
            my $i = 0;
            my $arg;
            for @$a -> $var {

                $arg = Val::Undef.new();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg = $var2[1];
                    }
                };

                my $bind = Bind.new( parameters => $var[1], arguments => $arg );
                $str = $str ~ ' ' ~ $bind.emit ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  Obj.new(:$a, :$b) = $obj

            my $class = $.parameters.class;
            my $a     = $.parameters.fields;
            my $b     = $.arguments;
            my $str   = 'do { ';
            my $i     = 0;
            my $arg;
            for @$a -> $var {
                my $bind = Bind.new( 
                    parameters => $var[1], 
                    arguments  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str = $str ~ ' ' ~ $bind.emit ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
    
        '(' ~ $.parameters.emit ~ ' = ' ~ $.arguments.emit ~ ')';
    }
}

class Proto {
    has $.name;
    method emit {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit {
        my $invocant = $.invocant.emit;
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
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
                return 
                    '[ map { Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    'Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')';
            }
        };
        if $.method eq 'push' { 
            return 'push( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit).join(', ') ~ ' )' 
        }
        if $.method eq 'unshift' { 
            return 'unshift( @{' ~ $invocant ~ '}, '~ (@.arguments.>>emit).join(', ') ~ ' )' 
        }
        if $.method eq 'pop' { 
            return 'pop( @{' ~ $invocant ~ '} )' 
        }
        if $.method eq 'shift' { 
            return 'shift( @{' ~ $invocant ~ '} )' 
        }

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth = '';  
        }
        
        my $call = '->' ~ $meth ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
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
    method emit {
        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        }

        if $code eq 'self'       { return '$self' };
        if $code eq 'False'      { return '0' };
        if $code eq 'True'       { return '1' };

        if $code eq 'make'       { return '($MATCH->{capture} = ('   ~ (@.arguments.>>emit).join(', ') ~ '))' };

        if $code eq 'say'        { return 'Main::say('   ~ (@.arguments.>>emit).join(', ') ~ ')' };
        if $code eq 'print'      { return 'Main::print(' ~ (@.arguments.>>emit).join(', ') ~ ')' };
        if $code eq 'warn'       { return 'warn('        ~ (@.arguments.>>emit).join(', ') ~ ')' };

        if $code eq 'array'      { return '@{' ~ (@.arguments.>>emit).join(' ')     ~ '}' };
        if $code eq 'pop'        { return 'pop( @{' ~ (@.arguments.>>emit).join(' ')  ~ '} )' };
        if $code eq 'push'       { return 'push( @{' ~ (@.arguments[0]).emit ~ '}, ' ~ (@.arguments[1]).emit ~ ' )' };
        if $code eq 'shift'      { return 'shift( @{' ~ (@.arguments.>>emit).join(' ')    ~ '} )' };

        if $code eq 'Int'        { return '(0+' ~ (@.arguments[0]).emit             ~ ')' };
        if $code eq 'Num'        { return '(0+' ~ (@.arguments[0]).emit             ~ ')' };
        if $code eq 'bool'       { return 'Main::bool('   ~ (@.arguments.>>emit).join(', ') ~ ')' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return '!'  ~ Perl5::to_bool(' && ', @.arguments) };
        if $code eq 'prefix:<?>' { return Perl5::to_bool(' && ', @.arguments) };

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit).join(' ')     ~ '}' };
        if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit).join(' ')     ~ ' || []}' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit).join(' ')     ~ '}' };

        if $code eq 'infix:<~>'  { return ''   ~ (@.arguments.>>emit).join(' . ')   ~ ''  };
        if $code eq 'infix:<+>'  { return '('  ~ (@.arguments.>>emit).join(' + ')   ~ ')' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit).join(' - ')   ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit).join(' * ')   ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit).join(' / ')   ~ ')' };
        if $code eq 'infix:<>>'  { return '('  ~ (@.arguments.>>emit).join(' > ')   ~ ')' };
        if $code eq 'infix:<<>'  { return '('  ~ (@.arguments.>>emit).join(' < ')   ~ ')' };
        if $code eq 'infix:<>=>' { return '('  ~ (@.arguments.>>emit).join(' >= ')  ~ ')' };
        if $code eq 'infix:<<=>' { return '('  ~ (@.arguments.>>emit).join(' <= ')  ~ ')' };
        if $code eq 'infix:<x>'  { return '('  ~ (@.arguments.>>emit).join(' x ')   ~ ')' };
        
        if $code eq 'infix:<&&>' { return Perl5::to_bool(' && ', @.arguments) };
        if $code eq 'infix:<||>' { return Perl5::to_bool(' || ', @.arguments) };
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit).join(' eq ')  ~ ')' };
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit).join(' ne ')  ~ ')' };
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit).join(' == ')  ~ ')' };
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit).join(' != ')  ~ ')' };

        if $code eq 'ternary:<?? !!>' { 
            my $cond = @.arguments[0];
            if   $cond.isa( 'Var' ) 
              && $cond.sigil eq '@' 
            {
                $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
            }
            return '(' ~ Perl5::to_bool(' && ', [$cond]) ~
                 ' ? ' ~ (@.arguments[1]).emit ~
                 ' : ' ~ (@.arguments[2]).emit ~
                  ')' };
        
        $code ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit {
        return 'return(' ~ $.result.emit ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit {
        my $cond = $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if = If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.emit;
        }
        return 'if (' ~ Perl5::to_bool(' && ', [$cond]) ~ ') { ' 
             ~   (@.body.>>emit).join(';') 
             ~ ' } ' 
             ~ 'else { ' 
             ~   (@.otherwise.>>emit).join(';') 
             ~ ' }';
    }
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
           'for ( '
        ~  ( $.init     ?? $.init.emit           ~ '; ' !! '; ' )
        ~  ( $cond      ?? Perl5::to_bool(' && ', [$cond]) ~ '; ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit       ~ ' '  !! ' '  )
        ~  ') { ' 
        ~       (@.body.>>emit).join('; ') 
        ~ ' }'
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        return  'for my ' ~ $.topic.emit ~ ' ( ' ~ $cond.emit ~ ' ) { ' 
             ~   (@.body.>>emit).join(';') 
             ~ ' }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit {
        my $decl = $.decl;
        my $name = $.var.plain_name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { $_[0]->{' ~ $name ~ '} }' )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = '';

        # $str = 'my $List__ = \\@_; ';   
        #
        # # TODO - follow recursively
        # for @$pos -> $field { 
        #    if ( $field.isa('Lit::Array') ) {
        #        $str = $str ~ 'my (' ~ (($field.array1).>>emit).join(', ') ~ '); ';
        #    }
        #    else {
        #        $str = $str ~ 'my ' ~ $field.emit ~ '; ';
        #    };
        # };
        #
        # my $bind = Bind.new( 
        #    parameters => Lit::Array.new( array1 => $sig.positional ), 
        #    arguments  => Var.new( sigil => '@', twigil => '', name => '_' )
        # );
        # $str = $str ~ $bind.emit ~ '; ';

        my $i = 1;
        for @$pos -> $field { 
            $str = $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i = $i + 1;
        }

        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
          $str ~
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = '';

        # my $str = 'my $List__ = \\@_; ';  
        #
        # # TODO - follow recursively
        # for @$pos -> $field { 
        #    if ( $field.isa('Lit::Array') ) {
        #        $str = $str ~ 'my (' ~ (($field.array1).>>emit).join(', ') ~ '); ';
        #    }
        #    else {
        #        $str = $str ~ 'my ' ~ $field.emit ~ '; ';
        #    };
        # };
        #
        # my $bind = Bind.new( 
        #    parameters => Lit::Array.new( array1 => $sig.positional ), 
        #    arguments  => Var.new( sigil => '@', twigil => '', name => '_' )
        # );
        # $str = $str ~ $bind.emit ~ '; ';

        my $i = 0;
        for @$pos -> $field { 
            $str = $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
            $i = $i + 1;
        }

        'sub ' ~ $.name ~ ' { ' ~ 
          $str ~
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Do {
    has @.block;
    method emit {
        'do { ' ~ 
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Use {
    has $.mod;
    method emit {
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

MiniPerl6::Perl5::Emit - Code generator for MiniPerl6-in-Perl5

=head1 SYNOPSIS

    $program.emit  # generated Perl5 code

=head1 DESCRIPTION

This module generates Perl5 code for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

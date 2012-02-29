use v6;

class CompUnit {
    has %.attributes;
    has %.methods;
    method emit {
        '# class ' ~ $.name ~ "; " ~ "\n" ~
        (@.body.>>emit).join( "; " ) ~ "\n";
    }
}

class Val::Int {
    method emit { $.int }
}

class Val::Bit {
    method emit { $.bit }
}

class Val::Num {
    method emit { $.num }
}

class Val::Buf {
    method emit { '\'' ~ $.buf ~ '\'' }
}

class Lit::Array {
    method emit {
        '[' ~ (@.array1.>>emit).join(', ') ~ ']';
    }
}

class Lit::Hash {
    method emit {
        my $fields = @.hash1;
        my $str = '';
        for @$fields -> $field { 
            $str = $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
    }
}

class Index {
    method emit {
        $.obj.emit ~ '.[' ~ $.index_exp.emit ~ ']';
    }
}

class Lookup {
    method emit {
        $.obj.emit ~ '.{' ~ $.index_exp.emit ~ '}';
    }
}

class Var {
    method emit {
        # Normalize the sigil here into $
        my $table = {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
           ( $.twigil eq '.' )
        ?? ( '$self.{' ~ $.name ~ '}' )
        !!  (    ( $.name eq '/' )
            ??   ( $table{$.sigil} ~ 'MATCH' )
            !!   ( $table{$.sigil} ~ $.name )
            )
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        $.parameters.emit ~ ' = ' ~ $.arguments.emit;
    }
}

class Proto {
    method emit {
        ~$.name        
    }
}

class Call {
    method emit {
        my $invocant = $.invocant.emit;
        if $invocant eq 'self' {
            $invocant = '$self';
        };
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
                return 
                    '[ map { &Main::' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')' ~ ' } @( ' ~ $invocant ~ ' ) ]';
            }
            else {
                return
                    '&Main::' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')';
            }
        };

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth = '';  
        };
        
        my $call = '.' ~ $meth ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        if ($.hyper) {
            '[ map { $_' ~ $call ~ ' } @( ' ~ $invocant ~ ' ) ]';
        }
        else {
            $invocant ~ $call;
        };

    }
}

class Apply {
    method emit {
        
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit ~ ').(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        };

        if $code eq 'self'       { return '$self' };

        if $code eq 'say'        { return 'say('   ~ (@.arguments.>>emit).join(', ') ~ ')' };
        if $code eq 'print'      { return 'print(' ~ (@.arguments.>>emit).join(', ') ~ ')' };

        if $code eq 'array'      { return '@(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ?? 0 !! 1)' };
        if $code eq 'prefix:<?>' { return '('  ~ (@.arguments.>>emit).join(' ')    ~ ' ?? 1 !! 0)' };

        if $code eq 'prefix:<$>' { return '$(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '@(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };
        if $code eq 'prefix:<%>' { return '%(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };

        if $code eq 'infix:<~>'  { return '('  ~ (@.arguments.>>emit).join(' ~ ')  ~ ')' };
        if $code eq 'infix:<+>'  { return '('  ~ (@.arguments.>>emit).join(' + ')  ~ ')' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit).join(' - ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit).join(' && ') ~ ')' };
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit).join(' || ') ~ ')' };
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit).join(' eq ') ~ ')' };
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit).join(' ne ') ~ ')' };
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit).join(' == ') ~ ')' };
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit).join(' != ') ~ ')' };

        if $code eq 'ternary:<?? !!>' { 
            return '(' ~ (@.arguments[0]).emit ~
                 ' ?? ' ~ (@.arguments[1]).emit ~
                 ' !! ' ~ (@.arguments[2]).emit ~
                  ')' };
        
        # TODO !!!
        '' ~ $.code ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        # '(' ~ $.code.emit ~ ').(' ~ @.arguments.>>emit.join(', ') ~ ')';
    }
}

class Return {
    method emit {
        return 'return(' ~ $.result.emit ~ ')';
    }
}

class If {
    method emit {
        'do { if (' ~ $.cond.emit ~ ') { ' ~ (@.body.>>emit).join(';') ~ ' } else { ' ~ (@.otherwise.>>emit).join(';') ~ ' } }';
    }
}

class For {
    method emit {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { for my ' ~ $.topic.emit ~ ' ( ' ~ $cond.emit ~ ' ) { ' ~ (@.body.>>emit).join(';') ~ ' } }';
    }
}

class Decl {
    method emit {
        return $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit;
    }
}

class Method {
    method emit {
        # TODO - signature binding
        my $sig = $.sig;
        # say "Sig: ", $sig.perl;
        my $invocant = $sig.invocant; 
        # say $invocant.emit;

        my $pos = $sig.positional;
        my $str = '';

        my $pos = $sig.positional;
        for @$pos -> $field { 
            $str = $str ~ '' ~ $field.emit ~ '?, ';
        };

        'method ' ~ $.name ~ '(' ~ $invocant.emit ~ ': ' ~ $str ~ ') { ' ~ 
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Sub {
    method emit {
        # TODO - signature binding
        my $sig = $.sig;
        # say "Sig: ", $sig.perl;
        my $pos = $sig.positional;
        my $str;

        my $pos = $sig.positional;
        for @$pos -> $field { 
            $str = $str ~ '' ~ $field.emit ~ '?, ';
        };

        if $.name eq '' {
            return 
                '(sub (' ~ $str ~ ') ' ~ ' { ' ~ 
                (@.block.>>emit).join('; ') ~ 
                ' })'
        }
    
        'sub ' ~ $.name ~ '(' ~ $str ~ ') ' ~ ' { ' ~ 
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Do {
    method emit {
        'do { ' ~ 
          (@.block.>>emit).join('; ') ~ 
        ' }'
    }
}

class Use {
    method emit {
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

Perlito6::Rakudo::Emit - Code generator for Perlito-in-Rakudo

=head1 SYNOPSIS

    $program.emit  # generated Perl6 code

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

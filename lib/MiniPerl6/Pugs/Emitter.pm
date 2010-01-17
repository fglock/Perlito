use v6;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit {
        'class ' ~ $.name ~ " { " ~ 
        (@.body.>>emit).join( "; " ) ~
        '} '
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
    method emit { '\'' ~ $.buf ~ '\'' }
}

class Val::Undef {
    method emit { '(undef)' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit {
        $.class.perl ~ '.new(' ~ %.fields.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method emit {
        '(' ~ (@.seq.>>emit).join(', ') ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit {
        '[' ~ (@.array1.>>emit).join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash1;
    method emit {
        my $fields := @.hash1;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        '{ ' ~ $str ~ ' }';
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit {
        # $.class ~ '->new( ' ~ @.fields.>>emit.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].emit ~ ' => ' ~ $_[1].emit}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).emit ~ ' => ' ~ ($field[1]).emit ~ ',';
        }; 
        $.class ~ '.new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index;
    method emit {
        $.obj.emit ~ '.[' ~ $.index.emit ~ ']';
        # TODO
        # if ($.obj.isa(Lit::Seq)) {
        #    $.obj.emit ~ '[' ~ $.index.emit ~ ']';
        # }
        # else {
        #    $.obj.emit ~ '->[' ~ $.index.emit ~ ']';
        # }
    }
}

class Lookup {
    has $.obj;
    has $.index;
    method emit {
        $.obj.emit ~ '.{' ~ $.index.emit ~ '}';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit {

        if ( $.name eq '/' ) {
            return '$MATCH';
        };
        return $.sigil ~ $.twigil ~ $.name;
    
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        #my $table := {
        #    '$' => '$',
        #    '@' => '$List_',
        #    '%' => '$Hash_',
        #    '&' => '$Code_',
        #};
        #   ( $.twigil eq '.' )
        #?? ( '$.' ~ $.name )  # ???
        #!!  (    ( $.name eq '/' )
        #    ??   ( $table{$.sigil} ~ '/' )  # ???
        #    !!   ( $table{$.sigil} ~ $.name )
        #    )
    };
    method name {
        $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] := [1, [2, 3]]
            
            my $a := $.parameters.array;
            #my $b := $.arguments.array;
            my $str := 'do { ';
            my $i := 0;
            for @$a -> $var { 
                my $bind := Bind.new( 
                    'parameters' => $var, 
                    # 'arguments' => ($b[$i]) );
                    'arguments'  => Index.new(
                        obj    => $.arguments,
                        index  => Val::Int.new( int => $i )
                    )
                );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} := { a => 1, b => [2, 3]}

            my $a := $.parameters.hash;
            my $b := $.arguments.hash;
            my $str := 'do { ';
            my $i := 0;
            my $arg;
            for @$a -> $var {

                $arg := Val::Undef.new();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg := $var2[1];
                    }
                };

                my $bind := Bind.new( 'parameters' => $var[1], 'arguments' => $arg );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  Obj.new(:$a, :$b) := $obj

            my $class := $.parameters.class;
            my $a     := $.parameters.fields;
            my $b     := $.arguments;
            my $str   := 'do { ';
            my $i     := 0;
            my $arg;
            for @$a -> $var {
                my $bind := Bind.new( 
                    'parameters' => $var[1], 
                    'arguments'  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.emit ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit ~ ' }';
        };
    
        $.parameters.emit ~ ' = ' ~ $.arguments.emit;
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
    #has $.hyper;
    method emit {
        my $invocant := $.invocant.emit;
        if $invocant eq 'self' {
            $invocant := 'self';  # ???
        };
        #if     ($.method eq 'perl')
        #    || ($.method eq 'yaml')
        #    || ($.method eq 'say' )
        #    || ($.method eq 'join')
        #    || ($.method eq 'chars')
        #    || ($.method eq 'isa')
        #{ 
        #    if ($.hyper) {
        #        return 
        #            $.method ~ '.>>(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')';
        #            #'[ map { ' ~ $.method ~ '( $_, ' ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
        #    }
        #    else {
        #        return
        #            $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit).join(', ') ~ ')';
        #    }
        #};

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
             $meth := '';  
        };
        
        my $call := '.' ~ $meth ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        if ($.hyper) {
            $invocant ~ '>>' ~ $call;
        }
        else {
            $invocant ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit {
        
        my $code := $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit ~ ').(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'self' };

        if $code eq 'say'        { return 'say('   ~ (@.arguments.>>emit).join(', ') ~ ')' };
        if $code eq 'print'      { return 'print(' ~ (@.arguments.>>emit).join(', ') ~ ')' };

        if $code eq 'array'      { return '@(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };

        if $code eq 'prefix:<~>' { return '~(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };
        if $code eq 'prefix:<!>' { return '!(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };
        if $code eq 'prefix:<?>' { return '?(' ~ (@.arguments.>>emit).join(' ')    ~ ')' };

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
        
        $.code ~ '(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        # '(' ~ $.code.emit ~ ')->(' ~ @.arguments.>>emit.join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit {
        'return(' ~ $.result.emit ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit {
        'do { if (' ~ $.cond.emit ~ ') { ' ~ (@.body.>>emit).join(';') ~ ' } else { ' ~ (@.otherwise.>>emit).join(';') ~ ' } }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit {
        my $cond := $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { for ( ' ~ $cond.emit ~ ' ) -> ' ~ $.topic.emit ~ ' { ' ~ (@.body.>>emit).join(';') ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit {
        my $decl := $.decl;
        my $name := $.var.name;
           ( $decl eq 'has' )
        ?? ( 'has $.' ~ $.var.name )
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
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit {
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        my $invocant := $sig.invocant; 
        # say $invocant.emit;

        my $pos := $sig.positional;
        my $str := '';

        # TODO - follow recursively
        my $pos := $sig.positional;
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit ~ '; ';
        };

        my $bind := Bind.new( 
            'parameters' => Lit::Array.new( array => $sig.positional ), 
            'arguments'  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit ~ '; ';

#        my $pos := $sig.positional;
#        my $str := '';
#        my $i := 1;
#        for @$pos -> $field { 
#            $str := $str ~ 'my ' ~ $field.emit ~ ' = $_[' ~ $i ~ ']; ';
#            $i := $i + 1;
#        };

        'method ' ~ $.name ~ ' { ' ~ 
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
        # TODO - signature binding
        my $sig := $.sig;
        # say "Sig: ", $sig.perl;
        ## my $invocant := $sig.invocant; 
        # say $invocant.emit;
        my $pos := $sig.positional;
        my $str := 'my $List__ = \@_; ';  # no strict "vars"; ';

        # TODO - follow recursively
        my $pos := $sig.positional;
        for @$pos -> $field { 
            $str := $str ~ 'my ' ~ $field.emit ~ '; ';
        };

        my $bind := Bind.new( 
            'parameters' => Lit::Array.new( array => $sig.positional ), 
            'arguments'  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.emit ~ '; ';

#        my $i := 0;
#        for @$pos -> $field { 
#            my $bind := Bind.new( 
#                'parameters' => $field, 
#                'arguments'  => Index.new(
#                        obj    => Var.new( sigil => '@', twigil => '', name => '_' ),
#                        index  => Val::Int.new( int => $i )
#                    ),
#                );
#            $str := $str ~ $bind.emit ~ '; ';
#            $i := $i + 1;
#        };
        'sub ' ~ $.name ~ ' { ' ~ 
          ## 'my ' ~ $invocant.emit ~ ' = $_[0]; ' ~
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

MiniPerl6::Pugs::Emit - Code generator for adapting MiniPerl6 dialect to Pugs

=head1 SYNOPSIS

    $program.emit  # generated Pugs code

=head1 DESCRIPTION

This module generates Perl5 code for the MiniPerl6 compiler.

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

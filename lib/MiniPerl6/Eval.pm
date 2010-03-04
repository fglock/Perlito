use v6;

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method eval ( $env ) {
        for @.body -> $stmt {
            $stmt.eval( $env );
        }
    }
}

class Val::Int {
    has $.int;
    method eval { $.int }
}

class Val::Bit {
    has $.bit;
    method eval { $.bit }
}

class Val::Num {
    has $.num;
    method eval { $.num }
}

class Val::Buf {
    has $.buf;
    method eval { $.buf }
}

class Val::Undef {
    method eval { undef }
}

class Val::Object {
    has $.class;
    has %.fields;
    method eval {
        warn "Interpreter TODO: Val::Object";
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method eval {
        warn "Interpreter TODO: Lit::Seq";
        '(' ~ (@.seq.>>eval).join(', ') ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method eval {
        warn "Interpreter TODO: Lit::Array";
        '[' ~ (@.array1.>>eval).join(', ') ~ ']';
    }
}

class Lit::Hash {
    has @.hash1;
    method eval {
        warn "Interpreter TODO: Lit::Hash";
        my $fields := @.hash1;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).eval ~ ' => ' ~ ($field[1]).eval ~ ',';
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
    method eval {
        warn "Interpreter TODO: Lit::Object";
        # $.class ~ '->new( ' ~ @.fields.>>eval.join(', ') ~ ' )';
        my $fields := @.fields;
        my $str := '';
        # say @fields.map(sub { $_[0].eval ~ ' => ' ~ $_[1].eval}).join(', ') ~ ')';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).eval ~ ' => ' ~ ($field[1]).eval ~ ',';
        }; 
        $.class ~ '->new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method eval {
        ( $.obj.eval )[ $.index_exp.eval ];
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method eval {
        ( $.obj.eval ){ $.index_exp.eval };
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method eval {
        warn "Interpreter TODO: Var";
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => '$',
            '@' => '$List_',
            '%' => '$Hash_',
            '&' => '$Code_',
        };
        my $ns := '';
        if $.namespace {
            $ns := $.namespace ~ '::';
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
    method eval {
        warn "Interpreter TODO: Bind";
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] := [1, [2, 3]]
            
            my $a := $.parameters.array1;
            #my $b := $.arguments.array1;
            my $str := 'do { ';
            my $i := 0;
            for @$a -> $var { 
                my $bind := Bind.new( 
                    parameters => $var, 
                    # arguments => ($b[$i]) );
                    arguments  => Index.new(
                        obj    => $.arguments,
                        index_exp  => Val::Int.new( int => $i )
                    )
                );
                $str := $str ~ ' ' ~ $bind.eval ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.eval ~ ' }';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} := { a => 1, b => [2, 3]}

            my $a := $.parameters.hash1;
            my $b := $.arguments.hash1;
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

                my $bind := Bind.new( parameters => $var[1], arguments => $arg );
                $str := $str ~ ' ' ~ $bind.eval ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.eval ~ ' }';
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
                    parameters => $var[1], 
                    arguments  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.eval ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.eval ~ ' }';
        };
    
        '(' ~ $.parameters.eval ~ ' = ' ~ $.arguments.eval ~ ')';
    }
}

class Proto {
    has $.name;
    method eval {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method eval {
        warn "Interpreter TODO: Call";
        my $invocant := $.invocant.eval;
        if $invocant eq 'self' {
            $invocant := '$self';
        }
        if ($.hyper) {
            # '[ map { $_' ~ $call ~ ' } @{ ' ~ $invocant ~ ' } ]';
        }
        else {
            # $invocant.$meth( @.arguments );
        }
        warn "Interpreter runtime error: method '", $.method, "()' not found";
    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method eval ($env) {
        my $ns := '';
        if $.namespace {
            $ns := $.namespace ~ '::';
        }
        my $code := $ns ~ $.code;
        for @($env) -> $e {
            if exists( $e{ $code } ) {
                return $e{ $code }.eval( $env, @.arguments );
            }
        }
        warn "Interpreter runtime error: subroutine '", $code, "()' not found";
    }
}

class Return {
    has $.result;
    method eval {
        warn "Interpreter TODO: Return";
        return
        #'do { print Main::perl(caller(),' ~ $.result.eval ~ '); return(' ~ $.result.eval ~ ') }';
        'return(' ~ $.result.eval ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method eval {
        warn "Interpreter TODO: If";
        my $cond := $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if := If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.eval;
        }
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { if (' ~ $cond.eval ~ ') { ' ~ (@.body.>>eval).join(';') ~ ' } else { ' ~ (@.otherwise.>>eval).join(';') ~ ' } }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method eval {
        warn "Interpreter TODO: For";
        my $cond := $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'do { for my ' ~ $.topic.eval ~ ' ( ' ~ $cond.eval ~ ' ) { ' ~ (@.body.>>eval).join(';') ~ ' } }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method eval {
        warn "Interpreter TODO: Decl";
        my $decl := $.decl;
        my $name := $.var.plain_name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}' )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.eval;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method eval {
        warn "Interpreter TODO: Sig";
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method eval {
        warn "Interpreter TODO: Method";
        my $sig := $.sig;
        my $invocant := $sig.invocant; 
        my $pos := $sig.positional;
        my $str := 'my $List__ = \\@_; ';   

        # TODO - follow recursively
        for @$pos -> $field { 
            if ( $field.isa('Lit::Array') ) {
                $str := $str ~ 'my (' ~ (($field.array1).>>eval).join(', ') ~ '); ';
            }
            else {
                $str := $str ~ 'my ' ~ $field.eval ~ '; ';
            };
        };

        my $bind := Bind.new( 
            parameters => Lit::Array.new( array1 => $sig.positional ), 
            arguments  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.eval ~ '; ';

        'sub ' ~ $.name ~ ' { ' ~ 
          'my ' ~ $invocant.eval ~ ' = shift; ' ~
          $str ~
          (@.block.>>eval).join('; ') ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method eval {
        warn "Interpreter TODO: Sub";
        my $sig := $.sig;
        my $pos := $sig.positional;
        my $str := 'my $List__ = \\@_; ';  

        # TODO - follow recursively
        for @$pos -> $field { 
            if ( $field.isa('Lit::Array') ) {
                $str := $str ~ 'my (' ~ (($field.array1).>>eval).join(', ') ~ '); ';
            }
            else {
                $str := $str ~ 'my ' ~ $field.eval ~ '; ';
            };
        };

        my $bind := Bind.new( 
            parameters => Lit::Array.new( array1 => $sig.positional ), 
            arguments  => Var.new( sigil => '@', twigil => '', name => '_' )
        );
        $str := $str ~ $bind.eval ~ '; ';

        'sub ' ~ $.name ~ ' { ' ~ 
          $str ~
          (@.block.>>eval).join('; ') ~ 
        ' }'
    }
}

class Do {
    has @.block;
    method eval {
        warn "Interpreter TODO: Do";
        'do { ' ~ 
          (@.block.>>eval).join('; ') ~ 
        ' }'
    }
}

class Use {
    has $.mod;
    method eval {
        warn "Interpreter TODO: Use";
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

MiniPerl6::Eval - AST interpreter for MiniPerl6

=head1 SYNOPSIS

    $program.eval  # runs program in interpreter

=head1 DESCRIPTION

This module executes MiniPerl6 AST in interpreter mode.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

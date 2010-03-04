use v6;

class EvalFunction {
    has $.func;

    method apply ( $env, $args ) {
        $.func.( $env, $args );
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method eval ($env) {
        my $env1 := [ {}, @$env ];
        for @.body -> $stmt {
            $stmt.eval($env1);
        }
    }
}

class Val::Int {
    has $.int;
    method eval ($env) { Int( $.int ) }
}

class Val::Bit {
    has $.bit;
    method eval ($env) { $.bit }
}

class Val::Num {
    has $.num;
    method eval ($env) { Num( $.num ) }
}

class Val::Buf {
    has $.buf;
    method eval ($env) { $.buf }
}

class Val::Undef {
    method eval ($env) { undef }
}

class Val::Object {
    has $.class;
    has %.fields;
    method eval ($env) {
        warn "Interpreter TODO: Val::Object";
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method eval ($env) {
        warn "Interpreter TODO: Lit::Seq";
    }
}

class Lit::Array {
    has @.array1;
    method eval ($env) {
        my @a;
        for @.array1 -> $v {
            push( @a, $v.eval($env) );
        }
        return @a;
    }
}

class Lit::Hash {
    has @.hash1;
    method eval ($env) {
        my %h;
        for @.hash1 -> $field { 
            %h{ ($field[0]).eval($env) } := ($field[1]).eval($env);
        }; 
        return %h;
    }
}

class Lit::Object {
    has $.class;
    has @.fields;
    method eval ($env) {
        warn "Interpreter TODO: Lit::Object";
        my $fields := @.fields;
        my $str := '';
        for @$fields -> $field { 
            $str := $str ~ ($field[0]).eval ~ ' => ' ~ ($field[1]).eval ~ ',';
        }; 
        $.class ~ '->new( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method eval ($env) {
        ( $.obj.eval($env) )[ $.index_exp.eval($env) ];
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method eval ($env) {
        ( $.obj.eval($env) ){ $.index_exp.eval($env) };
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method eval ($env) {
        my $ns := '';
        if $.namespace {
            $ns := $.namespace ~ '::';
        }
        else {
            if ($.sigil eq '@') && ($.twigil eq '*') && ($.name eq 'ARGS') {
                return @*ARGS
            }
            if $.twigil eq '.' {
                warn 'Interpreter TODO: $.' ~ $.name;
                return '$self->{' ~ $.name ~ '}' 
            }
            if $.name eq '/' {
                warn 'Interpreter TODO: $/';
                return $.sigil ~ 'MATCH' 
            }
        }

        my $name := $.sigil ~ $ns ~ $.name;
        for @($env) -> $e {
            if exists( $e{ $name } ) {
                return $e{ $name };
            }
        }
        warn "Interpreter runtime error: variable '", $name, "' not found";
    };
    method plain_name {
        if $.namespace {
            return $.sigil ~ $.namespace ~ '::' ~ $.name
        }
        return $.sigil ~ $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method eval ($env) {
        if $.parameters.isa( 'Lit::Array' ) {
            warn "Interpreter TODO: Bind";
        }
        if $.parameters.isa( 'Lit::Hash' ) {
            warn "Interpreter TODO: Bind";
        }
        if $.parameters.isa( 'Lit::Object' ) {
            warn "Interpreter TODO: Bind";
        }
        if $.parameters.isa( 'Decl' ) {
            $.parameters.eval($env);
        }
        my $name := $.parameters.plain_name;
        my $value := $.arguments.eval($env);
        for @($env) -> $e {
            if exists( $e{ $name } ) {
                $e{ $name } := $value;
                return $value;
            }
        }
        warn "Interpreter Bind: variable '" ~ $name ~ "' not found";
    }
}

class Proto {
    has $.name;
    method eval ($env) {
        ~$.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method eval ($env) {
        warn "Interpreter TODO: Call";
        my $invocant := $.invocant.eval($env);
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
        # warn "Apply ", $env.perl, " code: '", $code, "'";
        for @($env) -> $e {
            if exists( $e{ $code } ) {
                return $e{ $code }.apply( $env, @.arguments );
            }
        }
        warn "Interpreter runtime error: subroutine '", $code, "()' not found";
    }
}

class Return {
    has $.result;
    method eval ($env) {
        warn "Interpreter TODO: Return";
        return
        'return(' ~ $.result.eval ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method eval ($env) {
        my $cond := $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if := If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.eval($env);
        }
        # if   $cond.isa( 'Var' ) 
        #  && $cond.sigil eq '@' 
        # {
        #    $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        # }
        if $cond.eval($env) { 
            my $env1 := [ {}, @$env ];
            for @.body -> $stmt {
                $stmt.eval($env1);
            }
        } 
        else { 
            my $env1 := [ {}, @$env ];
            for @.otherwise -> $stmt {
                $stmt.eval($env1);
            }
        }
        return undef;
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method eval ($env) {
        my $cond := $.cond;
        # if   $cond.isa( 'Var' ) 
        #  && $cond.sigil eq '@' 
        # {
        #    $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        # }
        my $topic_name := $.topic.plain_name;
        my $env1 := [ {}, @$env ];
        for @( $cond.eval($env) ) -> $topic {
            $env1[0] := { $topic_name => $topic };
            for @.body -> $stmt {
                $stmt.eval($env1);
            }
        }
        return undef;
    }
}

class When {
    has @.parameters;
    has @.body;
    method eval { die "TODO - When" }
}

class While {
    has $.cond;
    has @.body;
    method eval { die "TODO - While" }
}

class Leave {
    method eval { die "TODO - Leave" }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method eval ($env) {
        my $decl := $.decl;
        my $name := $.var.plain_name;
        if $decl eq 'has' {
            warn "Interpreter TODO: has";
        }
        if !( exists ($env[0]){ $name } ) {
            ($env[0]){ $name } := undef;
        }
        return undef;
    }
    method plain_name {
        $.var.plain_name;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method eval ($env) {
        warn "Interpreter TODO: Sig";
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method eval ($env) {
        warn "Interpreter TODO: Method";
        my $sig := $.sig;
        my $invocant := $sig.invocant; 
        my $pos := $sig.positional;
        my $str := 'my $List__ = \\@_; ';   

        # ...
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method eval ($env) {
        my @param_name;
        for @( $.sig.positional ) -> $field { 
            push( @param_name, $field.plain_name );
        }
        my $sub :=  
            ::EvalFunction(
                func => sub ( $env, $args ) {
                    my %context;
                    my $n := 0;
                    %context{'@_'} := $args;
                    for @param_name -> $name {
                        %context{$name} := ($args[$n]).eval($env);
                        $n := $n + 1;
                    }
                    my $env1 := [ %context, @$env ];
                    my $r;
                    for @.block -> $stmt {
                        $r := $stmt.eval($env1);
                    }
                    return $r;
                },
            );
        if $.name {
            ($env[0]){$.name} := $sub;
        }
        return $sub;
    }
}

class Do {
    has @.block;
    method eval ($env) {
        my $env1 := [ {}, @$env ];
        for @.block -> $stmt {
            $stmt.eval($env1);
        }
    }
}

class Use {
    has $.mod;
    method eval ($env) {
        warn "Interpreter TODO: Use";
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

MiniPerl6::Eval - AST interpreter for MiniPerl6

=head1 SYNOPSIS

    $program_ast.eval($environment)  # runs program in interpreter

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

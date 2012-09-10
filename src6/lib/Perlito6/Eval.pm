use v6;

class CompUnit {
    method eval ($env) {
        my $env1 = [ {}, @$env ];
        for @.body -> $stmt {
            $stmt.eval($env1);
        }
    }
}

class Val::Int {
    method eval ($env) { Int( $.int ) }
}

class Val::Bit {
    method eval ($env) { $.bit }
}

class Val::Num {
    method eval ($env) { Num( $.num ) }
}

class Val::Buf {
    method eval ($env) { $.buf }
}

class Lit::Block {
    method eval ($env) {
        my $env1 = [ {}, @$env ];
        for @.stmts -> $stmt {
            $stmt.eval($env1);
        }
    }
}

class Lit::Array {
    method eval ($env) {
        my @a;
        for @.array1 -> $v {
            push( @a, $v.eval($env) );
        }
        return @a;
    }
}

class Lit::Hash {
    method eval ($env) {
        my %h;
        for @.hash1 -> $field { 
            my $pair = $field.arguments;
            %h{ ($pair[0]).eval($env) } = ($pair[1]).eval($env);
        }; 
        return %h;
    }
}

class Index {
    method eval ($env) {
        ( $.obj.eval($env) )[ $.index_exp.eval($env) ];
    }
}

class Lookup {
    method eval ($env) {
        ( $.obj.eval($env) ){ $.index_exp.eval($env) };
    }
}

class Var {
    method eval ($env) {
        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
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

        my $name = $.sigil ~ $ns ~ $.name;
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

class Proto {
    method eval ($env) {
        ~$.name        
    }
}

class Call {
    method eval ($env) {
        warn "Interpreter TODO: Call";
        my $invocant = $.invocant.eval($env);
        if $invocant eq 'self' {
            $invocant = '$self';
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
    method eval ($env) {
        my $ns = '';
        if $.namespace {
            $ns = $.namespace ~ '::';
        }
        my $code = $ns ~ $.code;
        # warn "Apply ", $env.perl, " code: '", $code, "'";
        for @($env) -> $e {
            if exists( $e{ $code } ) {
                return (($e{ $code }).( $env, @.arguments ));
            }
        }
        warn "Interpreter runtime error: subroutine '", $code, "()' not found";
    }
}

class If {
    method eval ($env) {
        my $cond = $.cond;
        if $cond.eval($env) { 
            my $env1 = [ {}, @$env ];
            for @(($.body).stmts) -> $stmt {
                $stmt.eval($env1);
            }
        } 
        else { 
            my $env1 = [ {}, @$env ];
            for @(($.otherwise).stmts) -> $stmt {
                $stmt.eval($env1);
            }
        }
        return Mu;
    }
}

class For {
    method eval ($env) {
        my $cond = $.cond;
        my $topic_name = (($.body).sig).plain_name;
        my $env1 = [ {}, @$env ];
        for @( $cond.eval($env) ) -> $topic {
            $env1[0] = { $topic_name => $topic };
            for @(($.body).stmts) -> $stmt {
                $stmt.eval($env1);
            }
        }
        return Mu;
    }
}

class When {
    method eval ($env) { die "TODO - When" }
}

class While {
    method eval ($env) { die "TODO - While" }
}

class Decl {
    method eval ($env) {
        my $decl = $.decl;
        my $name = $.var.plain_name;
        if $decl eq 'has' {
            warn "Interpreter TODO: has";
        }
        if !( exists ($env[0]){ $name } ) {
            ($env[0]){ $name } = Mu;
        }
        return Mu;
    }
    method plain_name {
        $.var.plain_name;
    }
}

class Method {
    method eval ($env) {
        warn "Interpreter TODO: Method";
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = 'my $List__ = \\@_; ';   

        # ...
    }
}

class Sub {
    method eval ($env) {
        my @param_name;
        for @( $.sig.positional ) -> $field { 
            push( @param_name, $field.plain_name );
        }
        my $sub =  
                sub ( $env, $args ) {
                    my %context;
                    my $n = 0;
                    %context{'@_'} = $args;
                    for @param_name -> $name {
                        %context{$name} = ($args[$n]).eval($env);
                        $n = $n + 1;
                    }
                    my $env1 = [ %context, @$env ];
                    my $r;
                    for @.block -> $stmt {
                        $r = $stmt.eval($env1);
                    }
                    return $r;
                };
        if $.name {
            ($env[0]){$.name} = $sub;
        }
        return $sub;
    }
}

class Do {
    method eval ($env) {
        my $env1 = [ {}, @$env ];
        for @.block -> $stmt {
            $stmt.eval($env1);
        }
    }
}

class Use {
    method eval ($env) {
        warn "Interpreter TODO: Use";
        'use ' ~ $.mod
    }
}

=begin

=head1 NAME 

Perlito6::Eval - AST interpreter for Perlito

=head1 SYNOPSIS

    $program_ast.eval($environment)  # runs program in interpreter

=head1 DESCRIPTION

This module executes Perlito AST in interpreter mode.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

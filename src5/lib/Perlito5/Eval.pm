use v5;

class CompUnit {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $env1 = [ {}, @$env ];
        for my $stmt ( @{$.body} ) {
            $stmt->eval($env1);
        }
    }
}

class Val::Int {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
        Int( $.int ) 
    }
}

class Val::Bit {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
        $.bit 
    }
}

class Val::Num {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
        Num( $.num ) 
    }
}

class Val::Buf {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
        $.buf 
    }
}

class Lit::Block {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $env1 = [ {}, @$env ];
        for my $stmt ( @{$.stmts} ) {
            $stmt->eval($env1);
        }
    }
}

class Lit::Array {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my @a;
        for my $v ( @{$.array1} ) {
            push( @a, $v->eval($env) );
        }
        return @a;
    }
}

class Lit::Hash {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my %h;
        for my $field ( @{$.hash1} ) {
            my $pair = $field->arguments;
            $h{ ($pair->[0])->eval($env) } = ($pair->[1])->eval($env);
        };
        return %h;
    }
}

class Index {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        ( $.obj->eval($env) )[ $.index_exp->eval($env) ];
    }
}

class Lookup {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        ( $.obj->eval($env) ){ $.index_exp->eval($env) };
    }
}

class Var {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $ns = '';
        if ($.namespace) {
            $ns = $.namespace . '::';
        }
        else {
            if ($.sigil eq '@') && ($.name eq 'ARGV') {
                return @ARGV
            }
            if ($.twigil eq '.') {
                warn 'Interpreter TODO: $.' . $.name;
                return '$self->{' . $.name . '}'
            }
            if ($.name eq 'MATCH') {
                warn 'Interpreter TODO: $MATCH';
                return $.sigil . 'MATCH'
            }
        }

        my $name = $.sigil . $ns . $.name;
        for my $e ( @($env) ) {
            if (exists( $e->{ $name } )) {
                return $e->{ $name };
            }
        }
        warn "Interpreter runtime error: variable '", $name, "' not found";
    };
    sub plain_name {
        my $self = $_[0];

        if ($.namespace) {
            return $.sigil . $.namespace . '::' . $.name
        }
        return $.sigil . $.name
    };
}

class Proto {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        '' . $.name
    }
}

class Call {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        warn "Interpreter TODO: Call";
        my $invocant = $.invocant->eval($env);
        if ($invocant eq 'self') {
            $invocant = '$self';
        }
        # $invocant.$meth( @{$.arguments} );
        warn "Interpreter runtime error: method '", $.method, "()' not found";
    }
}

class Apply {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $ns = '';
        if ($.namespace) {
            $ns = $.namespace . '::';
        }
        my $code = $ns . $.code;
        # warn "Apply ", $env->perl, " code: '", $code, "'";
        for my $e ( @($env) ) {
            if (exists( $e->{ $code } )) {
                return (($e->{ $code })->( $env, @{$.arguments} ));
            }
        }
        warn "Interpreter runtime error: subroutine '", $code, "()' not found";
    }
}

class If {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $cond = $.cond;
        if ($cond->eval($env)) {
            my $env1 = [ {}, @$env ];
            for my $stmt ( @(($.body)->stmts) ) {
                $stmt->eval($env1);
            }
        }
        else {
            my $env1 = [ {}, @$env ];
            for my $stmt ( @(($.otherwise)->stmts) ) {
                $stmt->eval($env1);
            }
        }
        return undef;
    }
}

class For {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $cond = $.cond;
        my $topic_name = (($.body)->sig)->plain_name;
        my $env1 = [ {}, @$env ];
        for my $topic (@( $cond->eval($env) )) {
            $env1->[0] = { $topic_name => $topic };
            for my $stmt ( @(($.body)->stmts) ) {
                $stmt->eval($env1);
            }
        }
        return undef;
    }
}

class When {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
 die "TODO - When" }
}

class While {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];
 die "TODO - While" }
}

class Decl {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $decl = $.decl;
        my $name = $.var->plain_name;
        if ($decl eq 'has') {
            warn "Interpreter TODO: has";
        }
        if (!( exists ($env->[0]){ $name } )) {
            ($env->[0]){ $name } = undef;
        }
        return undef;
    }
    sub plain_name {
        my $self = $_[0];

        $.var->plain_name;
    }
}

class Sub {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my @param_name;
        for my $field (@( $.sig->positional )) {
            push( @param_name, $field->plain_name );
        }
        my $sub =
                sub ( $env, $args ) {
                    my %context;
                    my $n = 0;
                    $context{'@_'} = $args;
                    for my $name ( @param_name ) {
                        $context{$name} = ($args->[$n])->eval($env);
                        $n = $n + 1;
                    }
                    my $env1 = [ %context, @$env ];
                    my $r;
                    for my $stmt ( @{$.block} ) {
                        $r = $stmt->eval($env1);
                    }
                    return $r;
                };
        if ($.name) {
            ($env->[0]){$.name} = $sub;
        }
        return $sub;
    }
}

class Do {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        my $env1 = [ {}, @$env ];
        for my $stmt ( @{$.block} ) {
            $stmt->eval($env1);
        }
    }
}

class Use {
    sub eval {
        my $self = $_[0];
        my $env = $_[1];

        warn "Interpreter TODO: Use";
        'use ' . $.mod
    }
}

=begin

=head1 NAME

Perlito5::Eval - AST interpreter for Perlito

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

Copyright 2010, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

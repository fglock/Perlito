use v5;

package CompUnit;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $env1 = [ {}, @$env ];
    for my $stmt ( @{$self->{"body"}} ) {
        $stmt->eval($env1);
    }
}

package Val::Int;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    0 + $self->{"int"} 
}


package Val::Num;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    0 + $self->{"num"} 
}


package Val::Buf;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    $self->{"buf"} 
}


package Lit::Block;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $env1 = [ {}, @$env ];
    for my $stmt ( @{$self->{"stmts"}} ) {
        $stmt->eval($env1);
    }
}


package Lit::Array;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my @a;
    for my $v ( @{$self->{"array1"}} ) {
        push( @a, $v->eval($env) );
    }
    return @a;
}

package Lit::Hash;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my %h;
    for my $field ( @{$self->{"hash1"}} ) {
        my $pair = $field->arguments;
        $h{ ($pair->[0])->eval($env) } = ($pair->[1])->eval($env);
    };
    return %h;
}


package Index;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    ( $self->{"obj"}->eval($env) )[ $self->{"index_exp"}->eval($env) ];
}


package Lookup;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    ( $self->{"obj"}->eval($env) ){ $self->{"index_exp"}->eval($env) };
}


package Var;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $ns = '';
    if ($self->{"namespace"}) {
        $ns = $self->{"namespace"} . '::';
    }
    else {
        if ($self->{"sigil"} eq '@') && ($self->{"name"} eq 'ARGV') {
            return @ARGV
        }
    }

    my $name = $self->{"sigil"} . $ns . $self->{"name"};
    for my $e ( @{$env} ) {
        if (exists( $e->{ $name } )) {
            return $e->{ $name };
        }
    }
    warn "Interpreter runtime error: variable '", $name, "' not found";
};
sub plain_name {
    my $self = $_[0];

    if ($self->{"namespace"}) {
        return $self->{"sigil"} . $self->{"namespace"} . '::' . $self->{"name"}
    }
    return $self->{"sigil"} . $self->{"name"}
};


package Proto;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    '' . $self->{"name"}
}


package Call;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    warn "Interpreter TODO: Call";
    my $invocant = $self->{"invocant"}->eval($env);
    if ($invocant eq 'self') {
        $invocant = '$self';
    }
    # $invocant.$meth( @{$self->{"arguments"}} );
    warn "Interpreter runtime error: method '", $self->{"method"}, "()' not found";
}


package Apply;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $ns = '';
    if ($self->{"namespace"}) {
        $ns = $self->{"namespace"} . '::';
    }
    my $code = $ns . $self->{"code"};
    # warn "Apply ", $env->perl, " code: '", $code, "'";
    for my $e ( @{$env} ) {
        if (exists( $e->{ $code } )) {
            return (($e->{ $code })->( $env, @{$self->{"arguments"}} ));
        }
    }
    warn "Interpreter runtime error: subroutine '", $code, "()' not found";
}


package If;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $cond = $self->{"cond"};
    if ($cond->eval($env)) {
        my $env1 = [ {}, @$env ];
        for my $stmt ( @{($self->{"body"})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    else {
        my $env1 = [ {}, @$env ];
        for my $stmt ( @{($self->{"otherwise"})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    return undef;
}


package For;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $cond = $self->{"cond"};
    my $topic_name = (($self->{"body"})->sig)->plain_name;
    my $env1 = [ {}, @$env ];
    for my $topic (@{ $cond->eval($env) }) {
        $env1->[0] = { $topic_name => $topic };
        for my $stmt ( @{($self->{"body"})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    return undef;
}


package When;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    die "TODO - When" 
}

package While;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    die "TODO - While" 
}

package Decl;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $decl = $self->{"decl"};
    my $name = $self->{"var"}->plain_name;
    if (!( exists ($env->[0]){ $name } )) {
        ($env->[0]){ $name } = undef;
    }
    return undef;
}
sub plain_name {
    my $self = $_[0];

    $self->{"var"}->plain_name;
}


package Sub;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my @param_name;
    for my $field (@{ $self->{"sig"}->positional }) {
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
                for my $stmt ( @{$self->{"block"}} ) {
                    $r = $stmt->eval($env1);
                }
                return $r;
            };
    if ($self->{"name"}) {
        ($env->[0]){$self->{"name"}} = $sub;
    }
    return $sub;
}


package Do;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $env1 = [ {}, @$env ];
    for my $stmt ( @{$self->{"block"}} ) {
        $stmt->eval($env1);
    }
}


package Use;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    warn "Interpreter TODO: Use";
    'use ' . $self->{"mod"}
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

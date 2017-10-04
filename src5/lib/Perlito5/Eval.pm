use v5;

package Perlito5::AST::CompUnit;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $env1 = [ {}, @$env ];
    for my $stmt ( @{$self->{body}} ) {
        $stmt->eval($env1);
    }
    return;
}

package Perlito5::AST::Int;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    0 + $self->{int} 
}


package Perlito5::AST::Num;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    0 + $self->{num} 
}


package Perlito5::AST::Buf;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    $self->{buf} 
}


package Perlito5::AST::Block;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $env1 = [ {}, @$env ];
    for my $stmt ( @{$self->{stmts}} ) {
        $stmt->eval($env1);
    }
}


package Perlito5::AST::Index;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    ( $self->{obj}->eval($env) )[ $self->{index_exp}->eval($env) ];
}


package Perlito5::AST::Lookup;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    ( $self->{obj}->eval($env) ){ $self->{index_exp}->eval($env) };
}


package Perlito5::AST::Var;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $ns = '';
    if ($self->{namespace}) {
        $ns = $self->{namespace} . '::';
    }
    else {
        if (($self->{sigil} eq '@') && ($self->{name} eq 'ARGV')) {
            return @ARGV
        }
    }

    my $name = $self->{sigil} . $ns . $self->{name};
    for my $e ( @{$env} ) {
        if (exists( $e->{ $name } )) {
            return $e->{ $name };
        }
    }
    warn "Interpreter runtime error: variable '", $name, "' not found";
}



package Perlito5::AST::Call;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    warn "Interpreter TODO: Perlito5::AST::Call";
    my $invocant = $self->{invocant}->eval($env);
    if ($invocant eq 'self') {
        $invocant = '$self';
    }
    # $invocant.$meth( @{$self->{arguments}} );
    warn "Interpreter runtime error: method '", $self->{method}, "()' not found";
}


package Perlito5::AST::Apply;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $ns = '';
    if ($self->{namespace}) {
        $ns = $self->{namespace} . '::';
    }
    my $code = $ns . $self->{code};
    for my $e ( @{$env} ) {
        if (exists( $e->{ $code } )) {
            return ($e->{ $code }->( $env, $self->{arguments} ));
        }
    }

    # # TODO - do BLOCK
    # my $env1 = [ {}, @$env ];
    # for my $stmt ( @{$self->{block}} ) {
    #     $stmt->eval($env1);
    # }

    warn "Interpreter runtime error: subroutine '", $code, "()' not found";
}


package Perlito5::AST::If;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $cond = $self->{cond};
    if ($cond->eval($env)) {
        my $env1 = [ {}, @$env ];
        for my $stmt ( @{($self->{body})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    else {
        my $env1 = [ {}, @$env ];
        for my $stmt ( @{($self->{otherwise})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    return undef;
}


package Perlito5::AST::For;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $cond = $self->{cond};
    my $topic_name = $self->{body}->sig->plain_name;
    my $env1 = [ {}, @$env ];
    for my $topic (@{ $cond->eval($env) }) {
        $env1->[0] = { $topic_name => $topic };
        for my $stmt ( @{($self->{body})->stmts} ) {
            $stmt->eval($env1);
        }
    }
    return undef;
}


package Perlito5::AST::When;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    die "TODO - When" 
}

package Perlito5::AST::While;
sub eval {
    my $self = $_[0];
    my $env = $_[1];
    die "TODO - Perlito5::AST::While" 
}

package Perlito5::AST::Decl;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $decl = $self->{decl};
    my $name = $self->{var}->plain_name;
    if (!exists( $env->[0]->{ $name } )) {
        $env->[0]{ $name } = undef;
    }
    return undef;
}


package Perlito5::AST::Sub;
sub eval {
    my $self = $_[0];
    my $env = $_[1];

    my $sub =
            sub {
                my $env = shift;
                my $args = shift;
                my %context;
                $context{'@_'} = $args;
                my $env1 = [ \%context, @$env ];
                my $r;
                for my $stmt ( @{$self->{block}} ) {
                    $r = $stmt->eval($env1);
                }
                return $r;
            };
    if ($self->{name}) {
        ($env->[0]){$self->{name}} = $sub;
    }
    return $sub;
}


1;

=begin

=head1 NAME

Perlito5::Eval - AST interpreter for Perlito

=head1 SYNOPSIS

    $program_ast.eval($environment)  # runs program in interpreter

=head1 DESCRIPTION

This module executes Perlito AST in interpreter mode.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

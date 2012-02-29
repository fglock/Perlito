package Perlito5::Grammar;

token unless {
    unless <.ws> <exp>
        {
            my $body = $MATCH->{"exp"}->flat()->{'end_block'};
            if (!defined($body)) {
                die "Missing code block in 'unless'";
            }
            $MATCH->{"capture"} = If->new(
                cond => $MATCH->{"exp"}->flat()->{'exp'},
                body => Lit::Block->new(stmts => [ ]),
                otherwise => $body,
             )
        }
}

token if {
    if <.ws> <exp>
    [
        <.opt_ws>
        else <exp2>
        {
            my $body = $MATCH->{"exp"}->flat()->{'end_block'};
            my $otherwise = ($MATCH->{"exp2"}->flat()){'exp'};
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            if (!(defined($otherwise))) {
                die "Missing code block in 'else'";
            }
            # if (ref($otherwise) eq 'Lit::Hash') {
            #     $otherwise = Lit::Block->new( stmts => $otherwise->hash1 );
            # }
            $MATCH->{"capture"} = If->new(
                cond      => $MATCH->{"exp"}->flat()->{'exp'},
                body      => $body,
                otherwise => $otherwise,
            )
        }
    |
        <.opt_ws>
        els <if>
        {
            my $body = $MATCH->{"exp"}->flat()->{'end_block'};
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = If->new(
                cond => $MATCH->{"exp"}->flat()->{'exp'},
                body => $body,
                otherwise => Lit::Block->new( stmts => [ $MATCH->{"if"}->flat() ] ),
            )
        }
    |
        {
            my $body = $MATCH->{"exp"}->flat()->{'end_block'};
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = If->new(
                cond => $MATCH->{"exp"}->flat()->{'exp'},
                body => $body,
                otherwise => Lit::Block->new(stmts => [ ]),
             )
        }
    ]
}

token when {
    when <.ws> <exp>
    {
        my $body = $MATCH->{"exp"}->flat()->{'end_block'};
        if (!defined($body)) {
            die "Missing code block in 'when'";
        }
        $MATCH->{"capture"} = When->new(
                parameters => $MATCH->{"exp"}->flat()->{'exp'},
                body       => $body )
    }
}

token for {
    for 'each'? <.ws> 
    [
        my <.ws> <Perlito5::Grammar.var_ident> <.opt_ws> 
            '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            $MATCH->{"capture"} = For->new( 
                    cond  => $MATCH->{"Perlito5::Expression.paren_parse"}->flat(), 
                    topic => undef, 
                    body  => Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat(), sig => $MATCH->{"Perlito5::Grammar.var_ident"}->flat() )
                 )
        }
    |
        <exp>
        {
            my $body = $MATCH->{"exp"}->flat()->{'end_block'};
            if (!defined($body)) {
                die "Missing code block in 'when'";
            }
            $MATCH->{"capture"} = For->new( cond => $MATCH->{"exp"}->flat()->{'exp'}, topic => undef, body => $body )
        }
    ]
}

token while {
    while <.ws> <exp>
    {
        my $body = $MATCH->{"exp"}->flat()->{'end_block'};
        if (!defined($body)) {
            die "Missing code block in 'while'";
        }
        $MATCH->{"capture"} = While->new(
                cond => $MATCH->{"exp"}->flat()->{'exp'},
                body => $body )
    }
}

=begin

=head1 NAME

Perlito5::Grammar - Grammar for Perlito

=head1 SYNOPSIS

    my $match = $source.parse;
    $match->flat();    # generated Perlito AST

=head1 DESCRIPTION

This module generates a syntax tree for the Perlito compiler.

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

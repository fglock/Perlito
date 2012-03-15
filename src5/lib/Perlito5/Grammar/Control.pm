package Perlito5::Grammar;

token unless {
    unless <.opt_ws> <Perlito5::Expression.term_paren>
       <.opt_ws> <Perlito5::Expression.term_curly>
    [
        <.opt_ws>
        else <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            my $body = $MATCH->{"Perlito5::Expression.term_curly"}->flat()->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = Perlito5::AST::If->new( 
                cond      => $MATCH->{"Perlito5::Expression.term_paren"}->flat()->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat() || [] ),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => $body),
            )
        }
    |
        {
            my $body = $MATCH->{"Perlito5::Expression.term_curly"}->flat()->[2];
            if (!defined($body)) {
                die "Missing code block in 'unless'";
            }
            $MATCH->{"capture"} = Perlito5::AST::If->new(
                cond      => $MATCH->{"Perlito5::Expression.term_paren"}->flat()->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => [ ]),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => $body),
             )
        }
    ]
};

token if {
    if <.opt_ws> <Perlito5::Expression.term_paren>
       <.opt_ws> <Perlito5::Expression.term_curly>
    [
        <.opt_ws>
        else <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            my $body = $MATCH->{"Perlito5::Expression.term_curly"}->flat()->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = Perlito5::AST::If->new( 
                cond      => $MATCH->{"Perlito5::Expression.term_paren"}->flat()->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat() || [] )
            )
        }
    |
        <.opt_ws>
        els <if>
        {
            my $body = $MATCH->{"Perlito5::Expression.term_curly"}->flat()->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = Perlito5::AST::If->new(
                cond      => $MATCH->{"Perlito5::Expression.term_paren"}->flat()->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => [ $MATCH->{"if"}->flat() ] ),
            )
        }
    |
        {
            my $body = $MATCH->{"Perlito5::Expression.term_curly"}->flat()->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{"capture"} = Perlito5::AST::If->new(
                cond      => $MATCH->{"Perlito5::Expression.term_paren"}->flat()->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => [ ]),
             )
        }
    ]
};

token when {
    when <.ws> <exp>
    {
        die "when - not implemented";

        my $body = $MATCH->{"exp"}->flat()->{'end_block'};
        if (!defined($body)) {
            die "Missing code block in 'when'";
        }
        $MATCH->{"capture"} = When->new(
                parameters => $MATCH->{"exp"}->flat()->{'exp'},
                body       => $body )
    }
};

token for {
    for 'each'?
    [
        <.ws> my <.opt_ws> <Perlito5::Grammar.var_ident> <.opt_ws> 
            '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            $MATCH->{"capture"} = Perlito5::AST::For->new( 
                    cond  => $MATCH->{"Perlito5::Expression.paren_parse"}->flat(), 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat(), sig => $MATCH->{"Perlito5::Grammar.var_ident"}->flat() )
                 )
        }
    |
        <.opt_ws>
            '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            $MATCH->{"capture"} = Perlito5::AST::For->new( 
                    cond  => $MATCH->{"Perlito5::Expression.paren_parse"}->flat(), 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat(), sig => undef )
                 )
        }
    ]
};

token while {
    while <.opt_ws>
            '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            $MATCH->{"capture"} = Perlito5::AST::While->new( 
                    cond  => $MATCH->{"Perlito5::Expression.paren_parse"}->flat(), 
                    body  => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat(), sig => undef )
                 )
        }
};

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

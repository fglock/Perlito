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
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}) || [] ),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => $body),
            )
        }
    |
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'unless'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
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
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}) || [] )
            )
        }
    |
        <.opt_ws>
        els <if>
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => [ Perlito5::Match::flat($MATCH->{if}) ] ),
            )
        }
    |
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
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

        my $body = Perlito5::Match::flat($MATCH->{exp})->{end_block};
        if (!defined($body)) {
            die "Missing code block in 'when'";
        }
        $MATCH->{capture} = When->new(
                parameters => Perlito5::Match::flat($MATCH->{exp})->{exp},
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
            '}' <.opt_ws>
        <opt_continue_block>
        {
            $MATCH->{capture} = Perlito5::AST::For->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.paren_parse"}), 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_ident"}) ),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
    |
        <.opt_ws>
            '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}' <.opt_ws>
        <opt_continue_block>
        {
            $MATCH->{capture} = Perlito5::AST::For->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.paren_parse"}), 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => undef ),
                    continue => $MATCH->{opt_continue_block}{capture}
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
            '}' <.opt_ws>
    <opt_continue_block>
        {
            $MATCH->{capture} = Perlito5::AST::While->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.paren_parse"}), 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => undef ),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
};

token opt_continue_block {
        'continue' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}'
        {
            $MATCH->{capture} = Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => undef )
        }
    |
        {
            $MATCH->{capture} = Perlito5::AST::Lit::Block->new( stmts => [], sig => undef )
        }
};

=begin

=head1 NAME

Perlito5::Grammar - Grammar for Perlito

=head1 SYNOPSIS

    my $match = $source.parse;
    Perlito5::Match::flat($match);    # generated Perlito AST

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

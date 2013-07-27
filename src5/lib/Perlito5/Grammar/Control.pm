package Perlito5::Grammar;
use strict;
use Perlito5::Expression;


Perlito5::Expression::add_statement( 'if'      => sub { Perlito5::Grammar->if( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'for'     => sub { Perlito5::Grammar->for( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'foreach' => sub { Perlito5::Grammar->for( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'when'    => sub { Perlito5::Grammar->when( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'while'   => sub { Perlito5::Grammar->while( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'given'   => sub { Perlito5::Grammar->given( $_[0], $_[1] ) } );
Perlito5::Expression::add_statement( 'unless'  => sub { Perlito5::Grammar->unless( $_[0], $_[1] ) } );


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
    when <.opt_ws> <Perlito5::Expression.term_paren>
         <.opt_ws> <Perlito5::Expression.term_curly>
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'when'";
            }
            $MATCH->{capture} = Perlito5::AST::When->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => $body),
             )
        }
};

token for {
    for 'each'?
    [
        <.ws> [ <Perlito5::Expression.term_declarator>
                { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Expression.term_declarator"})->[1] }
              | <Perlito5::Grammar.var_ident>
                { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_ident"}) }
              ]
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
                    body  => Perlito5::AST::Lit::Block->new( 
                                stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), 
                                sig   => $MATCH->{_tmp} 
                             ),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
    |
        <.opt_ws>

            # (@x)  (my $i = 0; $i < 10; $i++)

            '(' 
                <Perlito5::Expression.exp_parse>
                    [ ';' 
                          { $MATCH->{c_style_for} = 1 }
                          [ <Perlito5::Grammar.exp>  || <.opt_ws> ]
                      ';' [ <Perlito5::Grammar.exp2> || <.opt_ws> ]
                    | ''
                    ]
            ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts2>
                <.opt_ws>
            '}' <.opt_ws>
        <opt_continue_block>
        {
            my $header;
            if ($MATCH->{c_style_for}) {
                $header = [
                    $MATCH->{"Perlito5::Expression.exp_parse"}{capture},
                    $MATCH->{"Perlito5::Grammar.exp"}{capture},
                    $MATCH->{"Perlito5::Grammar.exp2"}{capture},
                ];
            }
            else {
                $header = $MATCH->{"Perlito5::Expression.exp_parse"}{capture};
            }

            $MATCH->{capture} = Perlito5::AST::For->new( 
                    cond  => $header, 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts2"}), sig => undef ),
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

token given {
    given <.opt_ws> '(' <Perlito5::Expression.paren_parse>   ')' <.opt_ws>
            '{' <.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.opt_ws>
            '}' <.opt_ws>
        {
            $MATCH->{capture} = Perlito5::AST::Given->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Expression.paren_parse"}), 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_ident"}) ),
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

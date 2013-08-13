package Perlito5::Grammar;
use strict;
use Perlito5::Grammar::Expression;


Perlito5::Grammar::Expression::add_statement( 'if'      => sub { Perlito5::Grammar->if( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'for'     => sub { Perlito5::Grammar->for( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'foreach' => sub { Perlito5::Grammar->for( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'when'    => sub { Perlito5::Grammar->when( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'while'   => sub { Perlito5::Grammar->while( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'given'   => sub { Perlito5::Grammar->given( $_[0], $_[1] ) } );
Perlito5::Grammar::Expression::add_statement( 'unless'  => sub { Perlito5::Grammar->unless( $_[0], $_[1] ) } );


token unless {
    'unless' <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_paren>
       <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_curly>
    [
        <.Perlito5::Grammar::Space.opt_ws>
        'else' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
            '}'
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}) || [] ),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => $body),
            )
        }
    |
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'unless'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => [ ]),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => $body),
             )
        }
    ]
};

token if {
    'if' <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_paren>
       <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_curly>
    [
        <.Perlito5::Grammar::Space.opt_ws>
        'else' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
            '}'
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}) || [] )
            )
        }
    |
        <.Perlito5::Grammar::Space.opt_ws>
        'els' <if>
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new( stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new( stmts => [ Perlito5::Match::flat($MATCH->{if}) ] ),
            )
        }
    |
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'if'";
            }
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => $body),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => [ ]),
             )
        }
    ]
};

token when {
    'when' <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_paren>
         <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Expression.term_curly>
        {
            my $body = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_curly"})->[2];
            if (!defined($body)) {
                die "Missing code block in 'when'";
            }
            $MATCH->{capture} = Perlito5::AST::When->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_paren"})->[2],
                body      => Perlito5::AST::Lit::Block->new(stmts => $body),
             )
        }
};

token for {
    'for' 'each'?
    [
        <.Perlito5::Grammar::Space.ws> [ <Perlito5::Grammar::Expression.term_declarator>
                { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.term_declarator"})->[1] }
              | <Perlito5::Grammar.var_ident>
                { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_ident"}) }
              ]
        <.Perlito5::Grammar::Space.opt_ws> 
            '(' <Perlito5::Grammar::Expression.paren_parse>   ')' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
            '}' <.Perlito5::Grammar::Space.opt_ws>
        <opt_continue_block>
        {
            $MATCH->{capture} = Perlito5::AST::For->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.paren_parse"}), 
                    topic => undef, 
                    body  => Perlito5::AST::Lit::Block->new( 
                                stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), 
                                sig   => $MATCH->{_tmp} 
                             ),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
    |
        <.Perlito5::Grammar::Space.opt_ws>

            # (@x)  (my $i = 0; $i < 10; $i++)

            '(' 
                <Perlito5::Grammar::Expression.exp_parse>
                    [ ';' 
                          { $MATCH->{c_style_for} = 1 }
                          [ <Perlito5::Grammar.exp>  || <.Perlito5::Grammar::Space.opt_ws> ]
                      ';' [ <Perlito5::Grammar.exp2> || <.Perlito5::Grammar::Space.opt_ws> ]
                    | ''
                    ]
            ')' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts2>
                <.Perlito5::Grammar::Space.opt_ws>
            '}' <.Perlito5::Grammar::Space.opt_ws>
        <opt_continue_block>
        {
            my $header;
            if ($MATCH->{c_style_for}) {
                $header = [
                    $MATCH->{"Perlito5::Grammar::Expression.exp_parse"}{capture},
                    $MATCH->{"Perlito5::Grammar.exp"}{capture},
                    $MATCH->{"Perlito5::Grammar.exp2"}{capture},
                ];
            }
            else {
                $header = $MATCH->{"Perlito5::Grammar::Expression.exp_parse"}{capture};
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
    'while' <.Perlito5::Grammar::Space.opt_ws>
            '(' <Perlito5::Grammar::Expression.paren_parse>   ')' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
            '}' <.Perlito5::Grammar::Space.opt_ws>
    <opt_continue_block>
        {
            $MATCH->{capture} = Perlito5::AST::While->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.paren_parse"}), 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => undef ),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
};

token given {
    'given' <.Perlito5::Grammar::Space.opt_ws> '(' <Perlito5::Grammar::Expression.paren_parse>   ')' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
            '}' <.Perlito5::Grammar::Space.opt_ws>
        {
            $MATCH->{capture} = Perlito5::AST::Given->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression.paren_parse"}), 
                    body  => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}), sig => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.var_ident"}) ),
                 )
        }
};


token opt_continue_block {
        'continue' <.Perlito5::Grammar::Space.opt_ws>
            '{' <.Perlito5::Grammar::Space.opt_ws>
                <Perlito5::Grammar.exp_stmts>
                <.Perlito5::Grammar::Space.opt_ws>
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

package Perlito5::Grammar;
use strict;
use Perlito5::Grammar::Expression;
use Perlito5::Grammar::Statement;

token unless {
    'unless' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
        <Perlito5::Grammar::Expression::term_paren>
        <.Perlito5::Grammar::Space::opt_ws> <block>
    [
        <.Perlito5::Grammar::Space::opt_ws>
        'else' <block2>
        {
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2],
                body      => Perlito5::Match::flat($MATCH->{block2}),
                otherwise => Perlito5::Match::flat($MATCH->{block}),
            )
        }
    |
        <.Perlito5::Grammar::Space::opt_ws>
        'els' <if_>
        {
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2],
                body      => Perlito5::AST::Block->new( stmts => [ Perlito5::Match::flat($MATCH->{if_}) ] ),
                otherwise => Perlito5::Match::flat($MATCH->{block}),
            )
        }
    |
        {
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::FoldConstant::fold_constant(Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2]),
                body      => Perlito5::AST::Block->new(stmts => [ ]),
                otherwise => Perlito5::Match::flat($MATCH->{block}),
             )
        }
    ]
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

token if_ {
    'if' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
        <Perlito5::Grammar::Expression::term_paren>
        <.Perlito5::Grammar::Space::opt_ws> <block>
    [
        <.Perlito5::Grammar::Space::opt_ws>
        'else' <block2>
        {
            $MATCH->{capture} = Perlito5::AST::If->new( 
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2],
                body      => Perlito5::Match::flat($MATCH->{block}),
                otherwise => Perlito5::Match::flat($MATCH->{block2}),
            )
        }
    |
        <.Perlito5::Grammar::Space::opt_ws>
        'els' <if_>
        {
            $MATCH->{capture} = Perlito5::AST::If->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2],
                body      => Perlito5::Match::flat($MATCH->{block}),
                otherwise => Perlito5::AST::Block->new( stmts => [ Perlito5::Match::flat($MATCH->{if_}) ] ),
            )
        }
    |
        {
            my $cond = Perlito5::FoldConstant::fold_constant(Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2]);
            if (Perlito5::FoldConstant::is_constant($cond) && !$cond->value) {
                # if-condition is always false
                $MATCH->{capture} = $cond;
            }
            else {
                $MATCH->{capture} = Perlito5::AST::If->new(
                    cond      => Perlito5::FoldConstant::fold_constant(Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2]),
                    body      => Perlito5::Match::flat($MATCH->{block}),
                    otherwise => Perlito5::AST::Block->new(stmts => [ ]),
                );
            }
        }
    ]
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

token when {
    'when' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
        <Perlito5::Grammar::Expression::term_paren>
        <.Perlito5::Grammar::Space::opt_ws> <block>
        {
            $MATCH->{capture} = Perlito5::AST::When->new(
                cond      => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_paren"})->[2],
                body      => Perlito5::Match::flat($MATCH->{block}),
             )
        }
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

sub is_bareword {
    my $term = shift;

    (ref($term) eq 'Perlito5::AST::Apply') and $term->{bareword};
}

token for {
    'for' 'each'?
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
    [
        [ <.Perlito5::Grammar::Space::ws> <Perlito5::Grammar::Expression::term_declarator>
            { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::term_declarator"})->[1] }
        | <.Perlito5::Grammar::Space::opt_ws> <before '$'> <Perlito5::Grammar::Sigil::term_sigil>
            { $MATCH->{_tmp} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Sigil::term_sigil"})->[1];

              my $v = $MATCH->{'_tmp'};
              my $look = Perlito5::Grammar::Scope::lookup_variable($v);
              # print STDERR "look: " . Dumper($look);
              my $decl = $look && $look->{_decl} ? $look->{_decl} : 'global';
              if ($decl ne 'global') {
                  # auto-insert a "declarator" (my, our, state)
                  $v->{_id} = $Perlito5::ID++;
                  $v->{_decl} = $decl;
                  # use Data::Dumper;
                  # print STDERR "variable: " . Dumper($v);
                  $MATCH->{'_tmp'} = Perlito5::AST::Decl->new(
                      decl => $decl,
                      var  => $v,
                  );
              }

            }
        ]
        <.Perlito5::Grammar::Space::opt_ws> 
            '(' <Perlito5::Grammar::Expression::paren_parse>   ')' <block> <opt_continue_block>
            {   my $body = Perlito5::Match::flat($MATCH->{block});
                
                my $header = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::paren_parse"});
                my $topic = $MATCH->{_tmp};
                my $continue_block = $MATCH->{opt_continue_block}{capture};
                
                $MATCH->{capture} = Perlito5::AST::For->new( 
                        cond  => $header,
                        body  => $body,
                        continue => $continue_block,
                        topic => $topic,
                     );
            }
    |
        <.Perlito5::Grammar::Space::opt_ws>

            # (@x)  (my $i = 0; $i < 10; $i++)

            '(' 
                [  <Perlito5::Grammar::Expression::exp_parse> 
                    { # register any loop variables, so they can be seen immediately
                      Perlito5::Grammar::Scope::check_variable_declarations();
                    }
                || [ <.Perlito5::Grammar::Space::opt_ws> <before ';'> ]
                ]
                    [ ';' 
                          { $MATCH->{c_style_for} = 1 }
                          [  <Perlito5::Grammar::exp>  
                             { # register any loop variables, so they can be seen immediately
                               Perlito5::Grammar::Scope::check_variable_declarations();
                             }
                          || <.Perlito5::Grammar::Space::opt_ws>
                          ]
                      ';' [ <Perlito5::Grammar::exp2> || <.Perlito5::Grammar::Space::opt_ws> ]
                    | ''
                    ]
            ')' <block> <opt_continue_block>
        {
            my $header;
            my $body = Perlito5::Match::flat($MATCH->{block});
            my $topic;
            my $continue_block = $MATCH->{opt_continue_block}{capture};
            if ($MATCH->{c_style_for}) {
                $header = [
                    $MATCH->{"Perlito5::Grammar::Expression::exp_parse"}{capture},
                    $MATCH->{"Perlito5::Grammar::exp"}{capture},
                    $MATCH->{"Perlito5::Grammar::exp2"}{capture},
                ];
            }
            else {
                $header = $MATCH->{"Perlito5::Grammar::Expression::exp_parse"}{capture};
                $topic  = Perlito5::AST::Var::SCALAR_ARG();
            }

            $MATCH->{capture} = Perlito5::AST::For->new( 
                    cond  => $header, 
                    body  => $body,
                    continue => $continue_block,
                    topic => $topic,
                 )
        }
    ]
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

token while {
    'while' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
            '(' <Perlito5::Grammar::Expression::paren_parse>   ')' <block> <opt_continue_block>
        {
            my $cond = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::paren_parse"});
            if ($cond eq '*undef*') {
                $cond = Perlito5::AST::Int->new( int => 1 );
            }
            my $stmt = Perlito5::AST::While->new( 
                    cond  => Perlito5::FoldConstant::fold_constant($cond), 
                    body  => Perlito5::Match::flat($MATCH->{block}),
                    continue => $MATCH->{opt_continue_block}{capture}
                 );
            my $out = Perlito5::Macro::while_file($stmt);
            $stmt = $out if $out;
            $MATCH->{capture} = $stmt;
        }
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

token until {
    'until' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
            '(' <Perlito5::Grammar::Expression::paren_parse>   ')' <block> <opt_continue_block>
        {
            my $cond = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::paren_parse"});
            if ($cond eq '*undef*') {
                $cond = Perlito5::AST::Int->new( int => 1 );
            }
            $MATCH->{capture} = Perlito5::AST::While->new( 
                    cond  => Perlito5::FoldConstant::fold_constant(
                                Perlito5::AST::Apply->new(
                                    'arguments' => [ $cond ],
                                    'code'      => 'prefix:<!>',
                                    'namespace' => '',
                                )
                            ),
                    body  => Perlito5::Match::flat($MATCH->{block}),
                    continue => $MATCH->{opt_continue_block}{capture}
                 )
        }
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};

token given {
    'given' <.Perlito5::Grammar::Space::opt_ws>
    { Perlito5::Grammar::Scope::create_new_compile_time_scope() }
        '(' <Perlito5::Grammar::Expression::paren_parse>   ')' <block>
        {
            my $body = Perlito5::Match::flat($MATCH->{block});
            $body->{sig} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::var_ident"});
            $MATCH->{capture} = Perlito5::AST::Given->new( 
                    cond  => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Expression::paren_parse"}), 
                    body  => $body,
                 )
        }
    { Perlito5::Grammar::Scope::end_compile_time_scope() }
};


Perlito5::Grammar::Statement::add_statement( 'if'      => \&if_ );
Perlito5::Grammar::Statement::add_statement( 'for'     => \&for );
Perlito5::Grammar::Statement::add_statement( 'foreach' => \&for );
Perlito5::Grammar::Statement::add_statement( 'when'    => \&when );
Perlito5::Grammar::Statement::add_statement( 'while'   => \&while );
Perlito5::Grammar::Statement::add_statement( 'until'   => \&until );
Perlito5::Grammar::Statement::add_statement( 'given'   => \&given );
Perlito5::Grammar::Statement::add_statement( 'unless'  => \&unless );


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
The Pugs Team.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

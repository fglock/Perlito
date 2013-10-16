package Perlito5::Perl6::TreeGrammar;
use Data::Dumper;
use strict;
use Perlito5::TreeGrammar;

sub refactor_while_glob {
    my ($class, $in) = @_;
    Perlito5::TreeGrammar::render(
        [   Ref => 'Perlito5::AST::While',
            [ Lookup => 'cond', 
                 [ And => [ Ref    => 'Perlito5::AST::Apply' ],
                          [ Lookup => 'code', [ Value => 'glob' ] ],
                          [ Action => sub { bless $in, 'Perlito5::AST::For' } ]
                 ]
            ]
        ],
        $in
    );
}

sub refactor_sub_arguments {
    my ($class, $in) = @_;
    my ( $rule, $result );
    
    $rule = Perlito5::Perl6::TreeGrammar::is_sub(
        [   Action => sub {
                my $sub = $_[0];
                my $stmts;
                my $var;
                Perlito5::TreeGrammar::render(
                    [   Lookup => 'block',
                        [   Progn => [ Action => sub { $stmts = $_[0] } ],
                                     [   Star => [
                                             Index => 0,
                                             Perlito5::Perl6::TreeGrammar::operator_eq( 'infix:<=>',
                                                 [   Lookup => 'arguments',
                                                     [   And => [   Index => 0,
                                                                    Perlito5::Perl6::TreeGrammar::my_var(
                                                                        [   Action => sub {
                                                                                $var = $_[0]->{var};
                                                                              }
                                                                        ]
                                                                    )
                                                                ],
                                                                [   Index => 1,
                                                                    Perlito5::Perl6::TreeGrammar::shift_arg()
                                                                ],
                                                                [   Action => sub {
                                                                        push @{ $sub->{args} }, $var;
                                                                        shift @$stmts;
                                                                      }
                                                                ]
                                                     ]
                                                 ]
                                             )
                                         ]
                                     ]
                        ]
                    ],
                    $sub
                );
              }
        ]
    );

    $result = Perlito5::TreeGrammar::scan( $rule, $in );
    # print "result $result\n";
    # print Dumper $in;
}

sub is_sub {
    [   Ref => 'Perlito5::AST::Sub',
        ( @_ ? [ Progn => @_ ] : () )
    ];
}

sub named_sub {
    [   Ref => 'Perlito5::AST::Sub',
        [   And => [ Lookup => 'name', [ Not => [ Value => '' ] ] ],
            ( @_ ? [ Progn => @_ ] : () )
        ]
    ];
}

sub operator_eq {
    my $name = shift;
    [   Ref => 'Perlito5::AST::Apply',
        [   And => [ Lookup => 'code', [ Value => $name ] ],
            ( @_ ? [ Progn => @_ ] : () )
        ]
    ];
}

sub my_var {
    [   Ref => 'Perlito5::AST::Decl',
        [   And => [ Lookup => 'decl', [ Value => 'my' ] ],
            ( @_ ? [ Progn => @_ ] : () )
        ]
    ];
}

sub var_is_at {
    [   Ref => 'Perlito5::AST::Var',
        [   And => [ Lookup => 'sigil', [ Value => '@' ] ],
                   [ Lookup => 'name',  [ Value => '_' ] ],
                   ( @_ ? [ Progn => @_ ] : () )
        ]
    ];
}

sub shift_arg {
    [   Ref => 'Perlito5::AST::Apply',
        [   And =>  [ Lookup => 'code', [ Value => 'shift' ] ],
                    [ Or => [ Lookup => 'arguments',
                                [ Not => [ Index => 0 ] ]   # arg list is empty
                            ],
                            [ Lookup => 'arguments',
                                [ Index => 0,
                                    var_is_at()             # arg is @_
                                ]
                            ],
                    ],
                    ( @_ ? [ Progn => @_ ] : () )
        ]
    ];
}

1;


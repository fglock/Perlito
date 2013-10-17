package main {
    use Data::Dumper;
    use strict;
    use Perlito5::TreeGrammar;

    my $in = eval join( '', <> );
    print Dumper $in;

    my ( $rule, $result );

    $rule = TreeGrammar::AST::is_sub(
        [   Action => sub {
                my $sub = $_[0];
                my $stmts;
                my $var;
                Perlito5::TreeGrammar::render(
                    [   Lookup => 'block',
                        [   Progn => [ Action => sub { $stmts = $_[0] } ],
                                     [   Star => [
                                             Index => 0,
                                             TreeGrammar::AST::operator_eq( 'infix:<=>',
                                                 [   Lookup => 'arguments',
                                                     [   And => [   Index => 0,
                                                                    TreeGrammar::AST::my_var(
                                                                        [   Action => sub {
                                                                                $var = $_[0]->{var};
                                                                              }
                                                                        ]
                                                                    )
                                                                ],
                                                                [   Index => 1,
                                                                    TreeGrammar::AST::shift_arg()
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
    print "result $result\n";
    print Dumper $in;

}

package TreeGrammar::AST {
    use strict;

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

    sub shift_arg {
        [   Ref => 'Perlito5::AST::Apply',
            [   And => [ Lookup => 'code', [ Value => 'shift' ] ],

                # TODO - bareword => 1, arguments => [], namespace => ''
                #     or arguments => [ @_ ]
                ( @_ ? [ Progn => @_ ] : () )
            ]
        ];
    }

}



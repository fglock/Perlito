package Perlito5::Perl6::TreeGrammar;
use Perlito5::Dumper;
use strict;
use Perlito5::TreeGrammar;

sub refactor_range_operator {
    my ($class, $in) = @_;
    Perlito5::TreeGrammar::render(
        [ And =>    [   Lookup => 'code', [ Value => 'infix:<..>' ] ],
                    [   Lookup => 'arguments', 
                        [ And =>  [ Index => 0,     [ And => [   Ref =>  'Perlito5::AST::Int' ],
                                                             [  Lookup => 'int', [ Value => 0 ] ]
                                                    ]
                                  ], # first argument is 0
                                  [ Index => 1, [ Or =>   
                                                    # 0 .. 9 ==> ^10
                                                    [ And => [   Ref =>  'Perlito5::AST::Int' ],
                                                             [ Action => sub {
                                                                 $in->{code} = 'p6_prefix:<^>';
                                                                 $_[0]{int}++;
                                                                 shift @{ $in->{arguments} };
                                                               }
                                                             ],
                                                    ],
                                                    # 0..$#$num to $num.keys
                                                    [ And => [   Ref =>  'Perlito5::AST::Apply' ],
                                                             [  Lookup => 'code', [ Value => 'prefix:<$#>' ] ],
                                                             [ Action => sub {
                                                                 bless $in, 'Perlito5::AST::Call';
                                                                 delete $in->{code};
                                                                 $in->{method} = 'keys';
                                                                 shift @{ $in->{arguments} };
                                                                 my $invocant = shift @{ $in->{arguments} };
                                                                 $in->{invocant} = $invocant->{arguments}[0];
                                                               }
                                                             ],
                                                    ],
                                                    # 0..$#num to @num.keys
                                                    [ And => [   Ref =>  'Perlito5::AST::Var' ],
                                                             [  Lookup => 'sigil', [ Value => '$#' ] ],
                                                             [ Action => sub {
                                                                 bless $in, 'Perlito5::AST::Call';
                                                                 delete $in->{code};
                                                                 $in->{method} = 'keys';
                                                                 $in->{arguments} = [];
                                                                 my $invocant = $_[0];
                                                                 $invocant->{sigil} = '@';
                                                                 $in->{invocant} = $invocant;
                                                               }
                                                             ],
                                                    ],
                                                ]
                                  ],
                        ]
                    ]
        ],
        $in
    );
}

sub refactor_while_glob {
    my ($class, $in) = @_;
    Perlito5::TreeGrammar::render(
        [   Ref => 'Perlito5::AST::While',
            [ Lookup => 'cond', 
                 [ And => [ Ref    => 'Perlito5::AST::Apply' ],
                          [ Lookup => 'code', [ Value => 'readline' ] ],
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


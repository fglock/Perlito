
package Perlito5::Expression;
use Perlito5::Precedence;
use Perlito5::Grammar;
use Perlito5::Grammar::Bareword;

sub expand_list {
    my $param_list = shift;
    # say "# expand_list: ", $param_list->perl;
    if ( ref( $param_list ) eq 'Perlito5::AST::Apply' && $param_list->code eq 'list:<,>') {
        my $args = [];
        for my $v ( @{$param_list->arguments} ) {
            if (defined($v)) {
                push( @$args, $v);
            }
        }
        return $args;
    }
    elsif ($param_list eq '*undef*') {
        return [];
    }
    else {
        return [ $param_list ];
    }
}

sub block_or_hash {
    my $o = shift;
    # say "# block_or_hash? ", $o->perl;
    if (defined($o->sig)) {
        # say "#  has sig -- not a block";
        return $o
    }
    my $stmts = $o->stmts;
    if (!(defined $stmts) || scalar(@$stmts) == 0) {
        # say "#  no contents -- empty hash";
        return Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => [] );
    }
    if (scalar(@$stmts) != 1) {
        # say "#  more statements -- not hash";
        return $o
    }
    my $stmt = $stmts->[0];
    if ( ref($stmt) eq 'Perlito5::AST::Var' ) {
        # the argument is a single variable
        # say "#  single var -- is hash";
        return Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => [ $stmt ])
    }
    if ( ref($stmt) ne 'Perlito5::AST::Apply' ) {
        # say "#  not Perlito5::AST::Apply -- not hash";
        return $o
    }
    if ($stmt->code eq 'infix:<=>>') {
        # the argument is a single pair
        # say "#  single pair -- is hash";
        return Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => [ $stmt ])
    }
    if ($stmt->code ne 'list:<,>') {
        # say "#  not a list -- not hash";
        return $o
    }
    # the argument is a list -- check that it contains a pair
    for my $item ( @{$stmt->arguments} ) {
        # say "#  item: ", $item->perl;
        if ( ref($item) eq 'Perlito5::AST::Apply' && $item->code eq 'infix:<=>>' ) {
            # argument is a pair
            # say "#  block: ", $o->perl;
            # say "#  hash with args: ", ( expand_list($stmt->arguments) )->perl;
            return Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => expand_list($stmt))
        }
    }
    return $o;
}

sub pop_term {
    my $num_stack = shift;
    my $v = pop @$num_stack;
    if (ref($v) eq 'ARRAY') {
        # say "# ** processing term ", $v->perl;
        if ($v->[1] eq 'methcall_no_params') {
            # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
            $v = Perlito5::AST::Call->new( invocant => undef, method => $v->[2], arguments => [] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'funcall_no_params') {
            # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
            $v = Perlito5::AST::Apply->new( code => $v->[3], namespace => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'methcall') {
            # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
            my $param_list = expand_list( ($v->[3]){'exp'} );
            $v = Perlito5::AST::Call->new( invocant => undef, method => $v->[2], arguments => $param_list );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'funcall') {
            # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
            my $param_list = expand_list( ($v->[4]){'exp'} );
            $v = Perlito5::AST::Apply->new( code => $v->[3], arguments => $param_list, namespace => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq '( )') {
            # say "#   Plain parentheses ", ($v->[2])->perl;
            my $param_list = expand_list($v->[2]);
            $v = Perlito5::AST::Apply->new( code => 'circumfix:<( )>', arguments => $param_list, namespace => '' );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq '[ ]') {
            # say "#   Array ", ($v->[2])->perl;
            my $param_list = expand_list($v->[2]);
            $v = Perlito5::AST::Apply->new( code => 'circumfix:<[ ]>', arguments => $param_list, namespace => '' );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'block') {
            # say "#   Block, Hash, or Pair ", ($v->[2])->perl;
            $v = Perlito5::AST::Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
            $v = block_or_hash($v);
            # TODO: $v = Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => $v->[2] );
            return $v;
        }
        if ($v->[1] eq '.( )') {
            # say "#   Params ", ($v->[2])->perl;
            # say "#     v:     ", $v->perl;
            $v = Perlito5::AST::Call->new( invocant => undef, method => 'postcircumfix:<( )>', arguments => $v->[2] );
            return $v;
        }
        if ($v->[1] eq '.[ ]') {
            # say "#   Perlito5::AST::Index ", ($v->[2])->perl;
            $v = Perlito5::AST::Index->new( obj => undef, index_exp => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq '.{ }') {
            # say "#   Perlito5::AST::Lookup ", ($v->[2])->perl;
            $v = Perlito5::AST::Lookup->new( obj => undef, index_exp => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if (ref($v->[1]) eq 'ARRAY' && scalar($v->[1]) == 2) {
            # say "#   old style Pair ", $v->perl;
            #   old style Pair - wrap it into a subroutine for now
            $v = Perlito5::AST::Apply->new( code => 'pair', arguments => $v->[1], namespace => '' );
            # say "#     ", $v->perl;
            return $v;
        }
        return $v->[1];
    }
    return $v;
}

sub reduce_postfix {
    my $op = shift;
    my $value = shift;
    my $v = $op;
    # say "# ** reduce_postfix ", $op->perl;
    # say "#      value: ", $value->perl;
    # say "#      v:     ", $v->perl;
    if ($v->[1] eq 'methcall_no_params') {
        # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
        $v = Perlito5::AST::Call->new( invocant => $value, method => $v->[2], arguments => [] );
        return $v;
    }
    if ($v->[1] eq 'funcall_no_params') {
        die "unexpected function call";
        # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
        push @$v, $value;
        return $v;
    }
    if ($v->[1] eq 'methcall') {
        # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
        my $param_list = expand_list($v->[3]{'exp'});
        $v = Perlito5::AST::Call->new( invocant => $value, method => $v->[2], arguments => $param_list );
        return $v;
    }
    if ($v->[1] eq 'funcall') {
        die "unexpected function call";
        # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
        push @$v, $value;
        return $v;
    }
    if ($v->[1] eq '( )') {
        # say "#   Params ", ($v->[2])->perl;
        my $param_list = expand_list($v->[2]);
        if ( ref($value) eq 'Perlito5::AST::Apply' && !(defined($value->arguments))) {
            $value->{'arguments'} = $param_list;
            return $value;
        }
        if ( ref($value) eq 'Perlito5::AST::Call' && !(defined($value->arguments))) {
            $value->{'arguments'} = $param_list;
            return $value;
        }
        $v = Perlito5::AST::Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list );
        return $v;
    }
    if ($v->[1] eq '[ ]') {
        # say "#   Perlito5::AST::Index ", ($v->[2])->perl;
        $v = Perlito5::AST::Index->new( obj => $value, index_exp => $v->[2] );
        # say "#     ", $v->perl;
        return $v;
    }
    if ($v->[1] eq 'block') {
        # say "#   Perlito5::AST::Lookup (was Block)", ($v->[2])->perl;
        $v = Perlito5::AST::Lookup->new( obj => $value, index_exp => ($v->[2])[0] );
        return $v;
    }
    if ($v->[1] eq '.( )') {
        my $param_list = expand_list($v->[2]);
        $v = Perlito5::AST::Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list );
        return $v;
    }
    if ($v->[1] eq '.[ ]') {
        $v = Perlito5::AST::Call->new( invocant => $value, method => 'postcircumfix:<[ ]>', arguments => $v->[2] );
        return $v;
    }
    if ($v->[1] eq '.{ }') {
        $v = Perlito5::AST::Call->new( invocant => $value, method => 'postcircumfix:<{ }>', arguments => $v->[2] );
        return $v;
    }
    push @$op, $value;
    return $op;
}

my $reduce_to_ast = sub {
    my $op_stack = shift;
    my $num_stack = shift;

    my $last_op = shift @$op_stack;
    # say "# reduce_to_ast ";
    # say "#     last_op: ", $last_op->perl;
    # say "#   num_stack: ", $num_stack;
    if ($last_op->[0] eq 'prefix') {
        push @$num_stack,
            Perlito5::AST::Apply->new(
                namespace => '',
                code      => 'prefix:<' . $last_op->[1] . '>',
                arguments => [ pop_term($num_stack) ],
              );
    }
    elsif ($last_op->[0] eq 'postfix') {
        push @$num_stack,
            Perlito5::AST::Apply->new(
                namespace => '',
                code      => 'postfix:<' . $last_op->[1] . '>',
                arguments => [ pop_term($num_stack) ],
              );
    }
    elsif ($last_op->[0] eq 'postfix_or_term') {
        push( @$num_stack,  reduce_postfix( $last_op, pop_term($num_stack) ) );
    }
    elsif (Perlito5::Precedence::is_assoc_type('list', $last_op->[1])) {
        my $arg;
        if (scalar(@$num_stack) < 2) {
            my $v2 = pop_term($num_stack);
            if ( ref($v2) eq 'Perlito5::AST::Apply' && $v2->code eq ('list:<' . $last_op->[1] . '>')) {
                push @$num_stack,
                    Perlito5::AST::Apply->new(
                        namespace => $v2->namespace,
                        code      => $v2->code,
                        arguments => [ @{ $v2->arguments }, undef ],
                      );
            }
            else {
                push @$num_stack,
                    Perlito5::AST::Apply->new(
                        namespace => '',
                        code      => 'list:<' . $last_op->[1] . '>',
                        arguments => [ $v2, undef ],
                      );
            }
            return;
        }
        else {
            my $v2 = pop_term($num_stack);
            $arg = [ pop_term($num_stack), $v2 ];
        }
        if  (  ref($arg->[0]) eq 'Perlito5::AST::Apply'
            && $last_op->[0] eq 'infix'
            && ($arg->[0]->code eq 'list:<' . $last_op->[1] . '>')
            )
        {
            push @$num_stack,
                Perlito5::AST::Apply->new(
                    namespace => '',
                    code      => ($arg->[0])->code,
                    arguments => [ @{ ($arg->[0])->arguments }, $arg->[1] ],
                  );
            return;
        }
        push @$num_stack,
            Perlito5::AST::Apply->new(
                namespace => '',
                code      => 'list:<' . $last_op->[1] . '>',
                arguments => $arg,
              );
    }
    elsif (Perlito5::Precedence::is_assoc_type('chain', $last_op->[1])) {
        if (scalar(@$num_stack) < 2) {
            die("Missing value after operator " . $last_op->[1]);
        }
        my $v2 = pop_term($num_stack);
        my $arg = [ pop_term($num_stack), $v2 ];
        # say "# assoc chain: ", $arg->perl;

        # TODO - create a special AST node for assoc chain?
        # if ($arg->[0])->isa('Perlito5::AST::Apply')
        #     && Perlito5::Precedence::is_assoc_type('chain', ($arg->[1]){op} )
        # {
        #     push @$num_stack,
        #         Perlito5::AST::Apply->new(
        #             namespace => '',
        #             code      => 'infix:<' . $last_op->[1] . '>',
        #             arguments => {
        #                 val   => [ $arg->[0] ],
        #                 chain => $arg->[1]
        #             }
        #         );
        #     return;
        # }
        push @$num_stack,
                Perlito5::AST::Apply->new(
                    namespace => '',
                    code      => 'infix:<' . $last_op->[1] . '>',
                    arguments => $arg
                );
    }
    elsif ($last_op->[0] eq 'ternary') {
        if ( scalar(@$num_stack) < 2 ) {
            die "Missing value after ternary operator";
        }
        my $v2 = pop_term($num_stack);
        push @$num_stack,
            Perlito5::AST::Apply->new(
                namespace => '',
                code      => 'ternary:<' . $last_op->[1] . '>',
                arguments => [ pop_term($num_stack), $last_op->[2], $v2 ],
              );
    }
    else {
        if ( scalar(@$num_stack) < 2 ) {
            die("missing value after operator '" . $last_op->[1] . "'");
        }
        my $v2 = pop_term($num_stack);
        push @$num_stack,
            Perlito5::AST::Apply->new(
                namespace => '',
                code      => 'infix:<' . $last_op->[1] . '>',
                arguments => [ pop_term($num_stack), $v2 ],
              );
    }
};

token term_arrow {
    '->' <.Perlito5::Grammar.opt_ws>
        [
        | '(' <paren_parse>   ')'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.( )',  $MATCH->{"paren_parse"}->flat()   ] }
        | '[' <square_parse>  ']'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.[ ]',  $MATCH->{"square_parse"}->flat()  ] }
        | '{' <curly_parse>   '}'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.{ }',  $MATCH->{"curly_parse"}->flat()   ] }

        | '$' <Perlito5::Grammar.ident> <.Perlito5::Grammar.opt_ws>
            [ '(' <paren_parse> ')'
              { $MATCH->{"capture"} = [ 'postfix_or_term',
                       'methcall',
                       Perlito5::AST::Var->new(
                               sigil       => '$',
                               namespace   => '',    # TODO - namespace
                               name        => $MATCH->{"Perlito5::Grammar.ident"}->flat(),
                           ),
                       { 
                         exp       => $MATCH->{"paren_parse"}->flat(),
                         terminated => 0,
                       },
                     ]
              }
            | { $MATCH->{"capture"} = [ 'postfix_or_term',
                       'methcall_no_params',
                       Perlito5::AST::Var->new(
                               sigil       => '$',
                               namespace   => '',    # TODO - namespace
                               name        => $MATCH->{"Perlito5::Grammar.ident"}->flat(),
                           ),
                      ]
              }
            ]


        | <Perlito5::Grammar.ident> <.Perlito5::Grammar.opt_ws>
            [ '(' <paren_parse> ')'
              { $MATCH->{"capture"} = [ 'postfix_or_term',
                       'methcall',
                       $MATCH->{"Perlito5::Grammar.ident"}->flat(),   # TODO - namespace
                       { 
                         exp       => $MATCH->{"paren_parse"}->flat(),
                         terminated => 0,
                       },
                     ]
              }
            | { $MATCH->{"capture"} = [ 'postfix_or_term',
                        'methcall_no_params',
                        $MATCH->{"Perlito5::Grammar.ident"}->flat()   # TODO - namespace
                      ]
              }
            ]
        ]
};


# the special variables list
# obtained with:
# $ perldoc -u perlvar | perl -ne ' /^\s*$/ && next; if (/^=item\s+([^\n]+)/) { push @item, $1; print "@item - $_" } else { if (@item) { push @xx, [@item]; print "push\n"; @item = () } }; END {use Data::Dumper; print Dumper \@xx} '

# $ perldoc -u perlvar | perl -ne ' /^\s*$/ && next; if (/^=item\s+([^\n]+)/) { push @item, $1; print "@item - $_" } else { if (@item) { push @xx, grep { /^[\@\$\%][^a-zA-Z0-9]$/ } @item; print "push\n"; @item = () } }; END {use Data::Dumper; print "$_  => 1,\n" for @xx} '

my %special_var = (
    '$_'  => 1,
    '$&'  => 1,
    '$`'  => 1,
    '$\''  => 1,
    '$+'  => 1,
    '@+'  => 1,
    '%+'  => 1,
    '$.'  => 1,
    '$/'  => 1,
    '$|'  => 1,
    '$,'  => 1,
    '$\\'  => 1,
    '$"'  => 1,
    '$;'  => 1,
    '$%'  => 1,
    '$='  => 1,
    '$-'  => 1,
    '@-'  => 1,
    '%-'  => 1,
    '$~'  => 1,
    '$^'  => 1,
    '$:'  => 1,
    '$?'  => 1,
    '$!'  => 1,
    '%!'  => 1,
    '$@'  => 1,
    '$$'  => 1,
    '$<'  => 1,
    '$>'  => 1,
    '$('  => 1,
    '$)'  => 1,
    '$['  => 1,
    '$]'  => 1,
    '@_'  => 1,
    '$#'  => 1,
    '$*'  => 1,

    '$#+'  => 1,    # $# + @+
    '$#-'  => 1,    # $# + @-
    '$#_'  => 1,    # $# + @_
);
sub term_special_var {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $len = 0;
    my $s = substr( $str, $pos, 3 );
    if ( exists $special_var{$s} ) {
        $len = 3;
    }
    else {
        $s = substr( $str, $pos, 2 );
        if ( exists $special_var{$s} ) {
            $len = 2;
        }
    }
    my $m = Perlito5::Match->new(
        str     => $str,
        from    => $pos,
        to      => $pos + $len,
        bool    => 0,
        capture => undef
    );
    if ( $len ) {
        my $c0 = substr( $str, $pos + $len - 1, 1 );
        my $c1 = substr( $str, $pos + $len, 1 );
        if  ( 
                ( $c0 eq '$' || $c0 eq '@' || $c0 eq '%' || $c0 eq '*' || $c0 eq '&' )
            &&  
                ( $c1 eq '$' || $c1 eq '@' || $c1 eq '%' || $c1 eq '*' || $c1 eq '&' 
                || ( $c1 ge 'a' && $c1 le 'z' )
                || ( $c1 ge 'A' && $c1 le 'Z' )
                || ( $c1 ge '0' && $c1 le '9' )
                )
            ) 
        {
            # TODO - this needs more testing
            # looks like a prefix operator, not a special var
        }
        else {
            $m->{"bool"} = 1;
            $m->{"capture"} = [ 'term', 
                                Perlito5::AST::Var->new(
                                        sigil       => substr($s, 0, $len - 1),
                                        namespace   => '',
                                        name        => substr($s, $len - 1, 1)
                                    )
                              ];
        }
    }
    return $m;
}

token var_sigil_or_pseudo     { '$#' | \$ |\% |\@ |\& | \* };

token term_sigil {
    <var_sigil_or_pseudo>
        [ '{' 
            [
            | <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.var_name> '}'
                    { $MATCH->{"capture"} = [ 'term', 
                            Perlito5::AST::Var->new(
                                    sigil       => $MATCH->{"var_sigil_or_pseudo"}->flat(),
                                    namespace   => $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat(),
                                    name        => $MATCH->{"Perlito5::Grammar.var_name"}->flat(),
                                )
                        ]
                    }
            | '^' <Perlito5::Grammar.var_name> '}'
                    { $MATCH->{"capture"} = [ 'term', 
                            Perlito5::AST::Var->new(
                                    sigil       => $MATCH->{"var_sigil_or_pseudo"}->flat(),
                                    namespace   => '',
                                    name        => '^' . $MATCH->{"Perlito5::Grammar.var_name"}->flat(),
                                )
                        ]
                    }
            | <curly_parse>   '}'
                { $MATCH->{"capture"} = [ 'term',  
                        Perlito5::AST::Apply->new( 
                                'arguments' => [ $MATCH->{"curly_parse"}->flat() ],
                                'code'      => 'prefix:<' . $MATCH->{"var_sigil_or_pseudo"}->flat() . '>', 
                                'namespace' => ''
                            )
                    ] 
                }
            ]
        | '^' <Perlito5::Grammar.word>
                { $MATCH->{"capture"} = [ 'term', 
                        Perlito5::AST::Var->new(
                                sigil       => $MATCH->{"var_sigil_or_pseudo"}->flat(),
                                namespace   => '',
                                name        => '^' . $MATCH->{"Perlito5::Grammar.word"}->flat(),
                            )
                    ]
                }
        | <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.var_name>
                { $MATCH->{"capture"} = [ 'term', 
                        Perlito5::AST::Var->new(
                                sigil       => $MATCH->{"var_sigil_or_pseudo"}->flat(),
                                namespace   => $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat(),
                                name        => $MATCH->{"Perlito5::Grammar.var_name"}->flat(),
                            )
                    ]
                }
        ]
    | <term_special_var>
            { $MATCH->{"capture"} = $MATCH->{"term_special_var"}->{"capture"} }
};

token term_digit {
      <Perlito5::Grammar.val_num>    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.val_num"}->flat() ]  }  # 123.456
    | <Perlito5::Grammar.val_int>    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.val_int"}->flat() ]  }  # 123
};

token term_ternary {
    '?'  <ternary5_parse> ':'
                { $MATCH->{"capture"} = [ 'op',          '? :', $MATCH->{"ternary5_parse"}->flat()  ] }
};

token term_paren {
    '('  <paren_parse>   ')'        { $MATCH->{"capture"} = [ 'postfix_or_term',  '( )',   $MATCH->{"paren_parse"}->flat()   ] }
};

token term_square {
    '['  <square_parse>  ']'      { $MATCH->{"capture"} = [ 'postfix_or_term',  '[ ]',   $MATCH->{"square_parse"}->flat()  ] }
};

token term_curly {
    '{'  <.Perlito5::Grammar.ws>?
           <Perlito5::Grammar.exp_stmts> <.Perlito5::Grammar.ws>? '}'
                { $MATCH->{"capture"} = [ 'postfix_or_term', 'block', $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat() ] }
};


token declarator {
     'my' | 'state' | 'our' | 'local'
};

token term_declarator {
    <declarator> <.Perlito5::Grammar.ws> <Perlito5::Grammar.opt_type> <.Perlito5::Grammar.opt_ws> <Perlito5::Grammar.var_ident>   # my Int $variable
        { $MATCH->{"capture"} = [ 'term', Perlito5::AST::Decl->new( decl => $MATCH->{"declarator"}->flat(), type => $MATCH->{"Perlito5::Grammar.opt_type"}->flat(), var => $MATCH->{"Perlito5::Grammar.var_ident"}->flat() ) ] }
};

token term_sub {
    'sub' <.Perlito5::Grammar.opt_ws> <Perlito5::Grammar.anon_sub_def>
                { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.anon_sub_def"}->flat()     ] }
};

token term_do {
    # Note: this is do-block; do-string is parsed as a normal subroutine
    'do' <.Perlito5::Grammar.ws> <before '{'> <statement_parse>
                { $MATCH->{"capture"} = [ 'term', Perlito5::AST::Do->new( block => $MATCH->{"statement_parse"}->flat() ) ] }
};

token use_decl { 'use' | 'no' };

token term_use {
    <use_decl> <.Perlito5::Grammar.ws>
        <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <list_parse>
        {
            $MATCH->{"capture"} = [ 'term', 
                Perlito5::AST::Use->new( 
                    code => $MATCH->{"use_decl"}->flat(),
                    mod  => $MATCH->{"Perlito5::Grammar.full_ident"}->flat() 
                )->compiletime_eval 
              ] 
        }
};

token term_package {
    'package' <.Perlito5::Grammar.ws> <Perlito5::Grammar.full_ident>
        {
            my $name = $MATCH->{"Perlito5::Grammar.full_ident"}->flat();
            $Perlito5::PKG_NAME = $name;
            $MATCH->{"capture"} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'package',
                    arguments => [], 
                    namespace => $name
                 )
               ]
        }
};

token term_eval {
    # Note: this is eval-block; eval-string is parsed as a normal subroutine
    'eval' <.Perlito5::Grammar.opt_ws> <before '{'> <term_curly>
        {
            $MATCH->{"capture"} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'eval',
                    arguments => [
                        Perlito5::AST::Do->new(
                            block => Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"term_curly"}->flat()->[2] ),
                        )
                    ], 
                    namespace => ''
                 )
               ]
        }
};

token map_or_sort { 'map' | 'sort' | 'grep' };

token term_map_or_sort {
    # Note: this is map-block; map-expr is parsed as a normal subroutine
    <map_or_sort> <.Perlito5::Grammar.opt_ws> <before '{'> <term_curly> 
        <list_parse>
        {
            $MATCH->{"capture"} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => $MATCH->{"map_or_sort"}->flat(),
                    arguments => [
                        Perlito5::AST::Lit::Block->new( stmts => $MATCH->{"term_curly"}->flat()->[2] ),
                        @{ expand_list($MATCH->{"list_parse"}->flat()->{"exp"}) }
                    ], 
                    namespace => ''
                 )
               ]
        }
};

token term_space {
    <.Perlito5::Grammar.ws>           { $MATCH->{"capture"} = [ 'space',   ' ' ] }
};

token has_newline_after {
    |    '#'
    |    <.Perlito5::Grammar.is_newline>
    |    <.Perlito5::Grammar.space>  <.has_newline_after>
};
token has_no_comma_or_colon_after {
    <.Perlito5::Grammar.ws> <!before [ ',' | ':' ]> .
};

my $Argument_end_token = {
    # 1 chars
        ':' => 1,
        ']' => 1,
        ')' => 1,
        '}' => 1,
        ';' => 1,
        ',' => 1,
        '<' => 1,   
        '>' => 1,   
        '=' => 1,   
        '&' => 1,   
        '|' => 1,   
        '^' => 1,   
      
    # 2 chars
        'or' => 1,
        'if' => 1,
        '=>' => 1,
        'lt' => 1,  
        'le' => 1,  
        'gt' => 1,  
        'ge' => 1,  
        '<=' => 1,  
        '>=' => 1,  
        '==' => 1,  
        '!=' => 1,  
        'ne' => 1,  
        'eq' => 1,  
        '..' => 1,  
        '~~' => 1,  
        '&&' => 1,  
        '||' => 1,  
        '+=' => 1,  
        '-=' => 1,  
        '*=' => 1,  
        '/=' => 1,  
        'x=' => 1,  
        '|=' => 1,  
        '&=' => 1,  
        '.=' => 1,  
        '^=' => 1,  
        '%=' => 1,  
        '//' => 1,  
     
    # 3 chars
        'for' => 1,
        'and' => 1,
        'xor' => 1,
        '...' => 1, 
        '<=>' => 1, 
        'cmp' => 1, 
        '<<=' => 1, 
        '>>=' => 1, 
        '||=' => 1, 
        '&&=' => 1, 
        '//=' => 1, 
        '**=' => 1, 
     
    # 4 chars
        # 'else' => 1,
        'when' => 1,
      
    # 5 chars
        'while' => 1,
        # 'elsif' => 1,
      
    # 6 chars
        'unless' => 1,
      
    # 7 chars
        'foreach' => 1,
      
};
my $Argument_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];


my $List_end_token = { 
    # 1 chars
        ':' => 1,
        ']' => 1,
        ')' => 1,
        '}' => 1,
        ';' => 1,
      
    # 2 chars
        'or' => 1,
        'if' => 1,
      
    # 3 chars
        'for' => 1,
        'and' => 1,
        'xor' => 1,
      
    # 4 chars
        'else' => 1,
        'when' => 1,
      
    # 5 chars
        'while' => 1,
        'elsif' => 1,
      
    # 6 chars
        'unless' => 1,
      
    # 7 chars
        'foreach' => 1,
      
};
my $List_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];

my $Expr_end_token = {
    # 1 chars
        ']' => 1,
        ')' => 1,
        '}' => 1,
        ';' => 1,
      
    # 2 chars
        'if' => 1,
      
    # 3 chars
        'for' => 1,
      
    # 4 chars
        'else' => 1,
        'when' => 1,
      
    # 5 chars
        'while' => 1,
        'elsif' => 1,
      
    # 6 chars
        'unless' => 1,
      
    # 7 chars
        'foreach' => 1,
      
};
my $Expr_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];


sub op_parse_spc {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $last_is_term = $_[3];

    my $m = Perlito5::Precedence->op_parse($str, $pos, $last_is_term);
    if (!$m->{"bool"}) {
        return $m;
    }
    my $spc = Perlito5::Grammar->ws($str, $m->{"to"});
    if ($spc->{"bool"}) {
        $m->{"to"} = $spc->{"to"};
    }
    return $m;
}

sub argument_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    # say "# argument_parse: input ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $is_first_token = 1;
    my $lexer_stack = [];
    my $terminated = 0;
    my $last_token_was_space = 1;
    my $last_is_term;
    my $get_token = sub {
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
        }
        else {
            my $m = Perlito5::Expression->op_parse_spc($str, $last_pos, $last_is_term);
            # say "# list lexer got: " . $m->perl;
            if (!$m->bool) {
                return [ 'end', '*end*' ];
            }
            $v = $m->flat();
            $last_is_term = Perlito5::Precedence::is_term($v) unless $v->[0] eq 'space';
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
            if ($v->[0] ne 'end') {
                $last_pos = $m->to;
            }
        }
        # say "# list_lexer got " . $v->perl;

        # say "# list_lexer " . $v->perl;

        if (   $v->[0] eq 'postfix_or_term'
            && $v->[1] eq 'block'
            && $last_token_was_space
           )
        {
            if ($self->has_newline_after($str, $last_pos)->bool) {
                # a block followed by newline terminates the expression
                $terminated = 1;
                push( @$lexer_stack,  [ 'end', '*end*' ] );
            }
            elsif ($self->has_no_comma_or_colon_after($str, $last_pos)->bool) {
                # a sequence ( block - space - not_comma_or_colon ) terminates the list
                $terminated = 1;
                push( @$lexer_stack,  [ 'end', '*end*' ] );
            }
        }
        $last_token_was_space = ($v->[0] eq 'space');
        $is_first_token = 0;

        return $v;
    };
    my $prec = Perlito5::Precedence->new(
        get_token       => $get_token, 
        reduce          => $reduce_to_ast,
        end_token       => $Argument_end_token,
        end_token_chars => $Argument_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# list_lexer return: ", $res->perl;
    if (scalar(@$res) == 0) {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
            capture => {
                exp        => '*undef*',
                terminated => undef } )
    }
    # if the expression terminates in a block, the block was pushed to num_stack
    my $block;
    if (scalar(@$res) > 1) {
        $block = pop @$res; # pop_term($res);
        $block = Perlito5::AST::Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
        # say "# list exp terminated with a block: ", $block->perl;
    }
    my $result = pop_term($res);
    if (scalar(@$res) > 0) {
        $block = pop @$res; # pop_term($res);
        $block = Perlito5::AST::Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
        # say "# list exp terminated with a block (2): ", $block->perl;
    }
    return Perlito5::Match->new(
        'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
        capture => {
            exp        => $result,
            terminated => $terminated } )
}


sub list_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    # say "# list_parse: input ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $is_first_token = 1;
    my $lexer_stack = [];
    my $terminated = 0;
    my $last_token_was_space = 1;
    my $last_is_term;
    my $get_token = sub {
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
        }
        else {
            my $m = Perlito5::Expression->op_parse_spc($str, $last_pos, $last_is_term);
            # say "# list lexer got: " . $m->perl;
            if (!$m->bool) {
                return [ 'end', '*end*' ];
            }
            $v = $m->flat();
            $last_is_term = Perlito5::Precedence::is_term($v) unless $v->[0] eq 'space';
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
            if ($v->[0] ne 'end') {
                $last_pos = $m->to;
            }
        }
        # say "# list_lexer got " . $v->perl;

        # say "# list_lexer " . $v->perl;
        
        $last_token_was_space = ($v->[0] eq 'space');
        $is_first_token = 0;

        return $v;
    };
    my $prec = Perlito5::Precedence->new(
        get_token       => $get_token, 
        reduce          => $reduce_to_ast,
        end_token       => $List_end_token,
        end_token_chars => $List_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# list_lexer return: ", $res->perl;
    if (scalar(@$res) == 0) {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
            capture => {
                exp        => '*undef*',
                terminated => undef } )
    }
    my $result = pop_term($res);
    return Perlito5::Match->new(
        'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
        capture => {
            exp        => $result,
            terminated => $terminated } )
}

sub circumfix_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
    my $delimiter = $_[3];
   
    # say "# circumfix_parse input: ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $last_is_term;
    my $get_token = sub {
        my $m = Perlito5::Expression->op_parse_spc($str, $last_pos, $last_is_term);
        if (!$m->bool) {
            die "Expected closing delimiter: ", $delimiter, ' near ', $last_pos;
        }
        my $v = $m->flat();
        $last_is_term = Perlito5::Precedence::is_term($v) unless $v->[0] eq 'space';
        if ($v->[0] ne 'end') {
            $last_pos = $m->to;
        }
        # say "# circumfix_lexer " . $v->perl;
        return $v;
    };

    my %delim_token;
    $delim_token{ $delimiter } = 1;
    my $prec = Perlito5::Precedence->new(
        get_token       => $get_token,
        reduce          => $reduce_to_ast,
        end_token       => \%delim_token,
        end_token_chars => [ length $delimiter ],
    );
    my $res = $prec->precedence_parse;
    $res = pop_term($res);
    # say "# circumfix_parse return: ", $res->perl;
    if (!(defined($res))) {
        # can't return undef in a capture (BUG in Match object?)
        $res = '*undef*';
    }
    return Perlito5::Match->new(
        'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
}

sub ternary5_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    return $self->circumfix_parse($str, $pos, ':');
}
sub curly_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    return $self->circumfix_parse($str, $pos, '}');
}
sub square_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    return $self->circumfix_parse($str, $pos, ']');
}
sub paren_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    return $self->circumfix_parse($str, $pos, ')');
}

sub exp_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    # say "# exp_parse input: ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $lexer_stack = [];
    my $terminated = 0;
    my $last_is_term;
    my $get_token = sub {
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
        }
        else {
            my $m = Perlito5::Expression->op_parse_spc($str, $last_pos, $last_is_term);
            # say "# exp lexer got: " . $m->perl;
            if (!$m->bool) {
                return [ 'end', '*end*' ];
            }
            $v = $m->flat();
            $last_is_term = Perlito5::Precedence::is_term($v) unless $v->[0] eq 'space';
            if ($v->[0] ne 'end') {
                $last_pos = $m->to;
            }
        }
        # say "# exp_lexer got " . $v->perl;
        return $v;
    };
    my $prec = Perlito5::Precedence->new(
        get_token       => $get_token,
        reduce          => $reduce_to_ast,
        end_token       => $Expr_end_token,
        end_token_chars => $Expr_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# exp terminated";
    if (scalar(@$res) == 0) {
        # say "# exp terminated with false";
        return Perlito5::Match->new(bool => 0);
    }
    my $result = pop_term($res);
    # say "# exp_parse result: ", $result->perl;
    return Perlito5::Match->new(
        'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
        capture => {
            exp        => $result,
            terminated => $terminated } )
}

token exp_stmt {
    | <Perlito5::Grammar.if>     { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.if"}->flat()     }
    | <Perlito5::Grammar.unless> { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.unless"}->flat() }
    | <Perlito5::Grammar.when>   { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.when"}->flat()   }
    | <Perlito5::Grammar.for>    { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.for"}->flat()    }
    | <Perlito5::Grammar.while>  { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.while"}->flat()  }
    | 'sub' <.Perlito5::Grammar.ws> <Perlito5::Grammar.named_sub_def>
                          { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.named_sub_def"}->flat() }
};

token statement_modifier {
    'if' | 'unless' | 'when' | 'foreach' | 'for' | 'while'
};

token delimited_statement {
    <.Perlito5::Grammar.ws>?
    [ ';' <.Perlito5::Grammar.ws>?
    | <statement_parse> ';'? <.Perlito5::Grammar.ws>?
        { $MATCH->{"capture"} = $MATCH->{"statement_parse"}->flat() }
    ]
};

sub statement_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    # say "# statement_parse input: ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $lexer_stack = [];
    my $res;

    # the rule for subroutines seems to be: 
    # named subs are statements,
    # anonymous subs are plain terms.

    $res = $self->exp_stmt($str, $pos);
    if ($res->{"bool"}) {
        # say "# statement result: ", $res->perl;
        return $res;
    }

    if ( substr($str, $pos, 1) eq '{' ) {
        # do we recognize a bare block in this position?
        # warn "maybe bareblock at $pos";
        my $m = $self->term_curly($str, $pos);
        if ($m->{"bool"}) {
            my $v = $m->flat();

            # TODO - this is not recognized as a statement: { 123 => 4;}
            # TODO - this is not recognized as a syntax error: { 123 => 4 }{2}

            $v = Perlito5::AST::Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
            $v = block_or_hash($v);

            if ( ref($v) eq 'Perlito5::AST::Lit::Block' ) {
                $m->{"capture"} = $v;
                return $m;
            }
        }
    }

    $res = $self->exp_parse($str, $pos);
    if (!$res->bool) {
        # say "# not a statement or expression";
        return $res;
    }
    if ( ref( $res->flat()->{'exp'} ) eq 'Perlito5::AST::Lit::Block' ) {
        # standalone block
        $res->flat()->{'exp'} = Perlito5::AST::Do->new(block => $res->flat()->{'exp'});
    }
    if ($res->flat()->{'terminated'}) {
        # say "# statement expression terminated result: ", $res->perl;
        $res->{"capture"} = $res->flat()->{'exp'};
        return $res;
    }
    # say "# look for a statement modifier";
    my $modifier = $self->statement_modifier($str, $res->to);
    if (!$modifier->bool) {
        # say "# statement expression no modifier result: ", $res->perl;
        # TODO - require a statement terminator
        $res->{"capture"} = $res->flat()->{'exp'};
        return $res;
    }
    my $modifier_exp = $self->exp_parse($str, $modifier->to);
    # say "# statement modifier [", $modifier->flat(), "] exp: ", $modifier_exp->perl;
    if (!$modifier_exp->bool) {
        die "Expected expression after '", $modifier->flat(), "'";
    }
    # TODO - require a statement terminator
    # say "# statement_parse modifier result: ", $modifier_exp->perl;

    $modifier = $modifier->flat();

    if ($modifier eq 'if') {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
            capture => Perlito5::AST::If->new(
                cond      => $modifier_exp->flat()->{'exp'},
                body      => Perlito5::AST::Lit::Block->new(stmts => [ $res->flat()->{'exp'} ]),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => [ ]) ) );
    }
    if ($modifier eq 'unless') {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
            capture => Perlito5::AST::If->new(
                cond      => $modifier_exp->flat()->{'exp'},
                body      => Perlito5::AST::Lit::Block->new(stmts => [ ]),
                otherwise => Perlito5::AST::Lit::Block->new(stmts => [ $res->flat()->{'exp'} ]) ) );
    }
    if ($modifier eq 'while') {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
            capture => Perlito5::AST::While->new(
                cond    => $modifier_exp->flat()->{'exp'},
                body    => Perlito5::AST::Lit::Block->new(stmts => [ $res->flat()->{'exp'} ] ) ) );
    }
    if  (  $modifier eq 'for'
        || $modifier eq 'foreach'
        ) 
    {
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
            capture => Perlito5::AST::For->new(
                cond    => $modifier_exp->flat()->{'exp'},
                body    => Perlito5::AST::Lit::Block->new(stmts => [ $res->flat()->{'exp'} ] ) ) );
    }
    die "Unexpected statement modifier '$modifier'";
}

1;

=begin

=head1 NAME

Perlito5::Expression - Parser and AST generator for Perlito

=head1 SYNOPSIS

    statement_parse($str)

=head1 DESCRIPTION

This module parses source code for Perl 5 statements and generates Perlito5 AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


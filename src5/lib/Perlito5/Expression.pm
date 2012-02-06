
package Perlito5::Expression;
    use Perlito5::Precedence;
    use Perlito5::Grammar;
    use Perlito5::Perl5::Emitter;

    sub expand_list {
        my $param_list = shift;
        # say "# expand_list: ", $param_list->perl;
        if ( ref( $param_list ) eq 'Apply' && $param_list->code eq 'list:<,>') {
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
            return Lit::Hash->new(hash1 => [])
        }
        if (scalar(@$stmts) != 1) {
            # say "#  more statements -- not hash";
            return $o
        }
        my $stmt = $stmts->[0];
        if ( ref($stmt) eq 'Var' ) {
            # the argument is a single variable
            # say "#  single var -- is hash";
            return Lit::Hash->new(hash1 => [ $stmt ])
        }
        if ( ref($stmt) ne 'Apply' ) {
            # say "#  not Apply -- not hash";
            return $o
        }
        if ($stmt->code eq 'infix:<=>>') {
            # the argument is a single pair
            # say "#  single pair -- is hash";
            return Lit::Hash->new(hash1 => [ $stmt ])
        }
        if ($stmt->code ne 'list:<,>') {
            # say "#  not a list -- not hash";
            return $o
        }
        # the argument is a list -- check that it contains a pair
        for my $item ( @{$stmt->arguments} ) {
            # say "#  item: ", $item->perl;
            if ( ref($item) eq 'Apply' && $item->code eq 'infix:<=>>' ) {
                # argument is a pair
                # say "#  block: ", $o->perl;
                # say "#  hash with args: ", ( expand_list($stmt->arguments) )->perl;
                return Lit::Hash->new(hash1 => expand_list($stmt))
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
                # say "#   Call ", ($v->[2])->perl;
                $v = Call->new( invocant => undef, method => $v->[2], arguments => [] );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq 'funcall_no_params') {
                # say "#   Apply ", ($v->[2])->perl;
                $v = Apply->new( code => $v->[3], namespace => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq 'methcall') {
                # say "#   Call ", ($v->[2])->perl;
                if ($v->[3]){'end_block'} {
                    # say "# pop_term: found end_block in Call";
                    unshift( @$num_stack, ($v->[3]){'end_block'} );
                }
                my $param_list = expand_list( ($v->[3]){'exp'} );
                $v = Call->new( invocant => undef, method => $v->[2], arguments => $param_list );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq 'funcall') {
                # say "#   Apply ", ($v->[2])->perl;
                if ($v->[4]){'end_block'} {
                    # say "# pop_term: found end_block in Apply";
                    unshift( @$num_stack, ($v->[4]){'end_block'} );
                }
                my $param_list = expand_list( ($v->[4]){'exp'} );
                $v = Apply->new( code => $v->[3], arguments => $param_list, namespace => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq '( )') {
                # say "#   Plain parentheses ", ($v->[2])->perl;
                my $param_list = expand_list($v->[2]);
                $v = Apply->new( code => 'circumfix:<( )>', arguments => $param_list, namespace => '' );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq '[ ]') {
                # say "#   Array ", ($v->[2])->perl;
                my $param_list = expand_list($v->[2]);
                $v = Lit::Array->new( array1 => $param_list );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq 'block') {
                # say "#   Block, Hash, or Pair ", ($v->[2])->perl;
                $v = Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
                $v = block_or_hash($v);
                # TODO: $v = Lit::Hash->new( hash1 => $v->[2] );
                return $v;
            }
            if ($v->[1] eq '.( )') {
                # say "#   Params ", ($v->[2])->perl;
                # say "#     v:     ", $v->perl;
                $v = Call->new( invocant => undef, method => 'postcircumfix:<( )>', arguments => $v->[2] );
                return $v;
            }
            if ($v->[1] eq '.[ ]') {
                # say "#   Index ", ($v->[2])->perl;
                $v = Index->new( obj => undef, index_exp => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1] eq '.{ }') {
                # say "#   Lookup ", ($v->[2])->perl;
                $v = Lookup->new( obj => undef, index_exp => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if (ref($v->[1]) eq 'ARRAY' && scalar($v->[1]) == 2) {
                # say "#   old style Pair ", $v->perl;
                #   old style Pair - wrap it into a subroutine for now
                $v = Apply->new( code => 'pair', arguments => $v->[1], namespace => '' );
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
            # say "#   Call ", ($v->[2])->perl;
            $v = Call->new( invocant => $value, method => $v->[2], arguments => [] );
            return $v;
        }
        if ($v->[1] eq 'funcall_no_params') {
            die "unexpected function call";
            # say "#   Apply ", ($v->[2])->perl;
            push @$v, $value;
            return $v;
        }
        if ($v->[1] eq 'methcall') {
            # say "#   Call ", ($v->[2])->perl;
            my $param_list = expand_list($v->[3]{'exp'});
            $v = Call->new( invocant => $value, method => $v->[2], arguments => $param_list );
            return $v;
        }
        if ($v->[1] eq 'funcall') {
            die "unexpected function call";
            # say "#   Apply ", ($v->[2])->perl;
            push @$v, $value;
            return $v;
        }
        if ($v->[1] eq '( )') {
            # say "#   Params ", ($v->[2])->perl;
            my $param_list = expand_list($v->[2]);
            if ( ref($value) eq 'Apply' && !(defined($value->arguments))) {
                $value->{'arguments'} = $param_list;
                return $value;
            }
            if ( ref($value) eq 'Call' && !(defined($value->arguments))) {
                $value->{'arguments'} = $param_list;
                return $value;
            }
            $v = Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list );
            return $v;
        }
        if ($v->[1] eq '[ ]') {
            # say "#   Index ", ($v->[2])->perl;
            $v = Index->new( obj => $value, index_exp => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'block') {
            # say "#   Lookup (was Block)", ($v->[2])->perl;
            $v = Lookup->new( obj => $value, index_exp => ($v->[2])[0] );
            return $v;
        }
        if ($v->[1] eq '.( )') {
            my $param_list = expand_list($v->[2]);
            $v = Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list );
            return $v;
        }
        if ($v->[1] eq '.[ ]') {
            $v = Call->new( invocant => $value, method => 'postcircumfix:<[ ]>', arguments => $v->[2] );
            return $v;
        }
        if ($v->[1] eq '.{ }') {
            $v = Call->new( invocant => $value, method => 'postcircumfix:<{ }>', arguments => $v->[2] );
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
                Apply->new(
                    namespace => '',
                    code      => 'prefix:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif ($last_op->[0] eq 'postfix') {
            push @$num_stack,
                Apply->new(
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
                if ( ref($v2) eq 'Apply' && $v2->code eq ('list:<' . $last_op->[1] . '>')) {
                    push @$num_stack,
                        Apply->new(
                            namespace => $v2->namespace,
                            code      => $v2->code,
                            arguments => [ @{ $v2->arguments }, undef ],
                          );
                }
                else {
                    push @$num_stack,
                        Apply->new(
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
            if  (  ref($arg->[0]) eq 'Apply'
                && $last_op->[0] eq 'infix'
                && ($arg->[0]->code eq 'list:<' . $last_op->[1] . '>')
                )
            {
                push @$num_stack,
                    Apply->new(
                        namespace => '',
                        code      => ($arg->[0])->code,
                        arguments => [ @{ ($arg->[0])->arguments }, $arg->[1] ],
                      );
                return;
            }
            push @$num_stack,
                Apply->new(
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
            # if ($arg->[0])->isa('Apply')
            #     && Perlito5::Precedence::is_assoc_type('chain', ($arg->[1]){op} )
            # {
            #     push @$num_stack,
            #         Apply->new(
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
                    Apply->new(
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
                Apply->new(
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
                Apply->new(
                    namespace => '',
                    code      => 'infix:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack), $v2 ],
                  );
        }
    };

    token operator {

        | <Perlito5::Grammar.var_sigil> '{' <curly_parse>   '}'
                    { $MATCH->{"capture"} = [ 'term',  
                            Apply->new( 
                                'arguments' => [ $MATCH->{"curly_parse"}->flat() ],
                                'code' => 'prefix:<' . $MATCH->{"Perlito5::Grammar.var_sigil"}->flat() . '>', 
                                'namespace' => ''
                            )   
                        ] 
                    }
        | '->'
            [
            | '(' <paren_parse>   ')'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.( )',  $MATCH->{"paren_parse"}->flat()   ] }
            | '[' <square_parse>  ']'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.[ ]',  $MATCH->{"square_parse"}->flat()  ] }
            | '{' <curly_parse>   '}'                   { $MATCH->{"capture"} = [ 'postfix_or_term',  '.{ }',  $MATCH->{"curly_parse"}->flat()   ] }
            ]
        | '('  <paren_parse>   ')'                      { $MATCH->{"capture"} = [ 'postfix_or_term',  '( )',   $MATCH->{"paren_parse"}->flat()   ] }
        | '['  <square_parse>  ']'                      { $MATCH->{"capture"} = [ 'postfix_or_term',  '[ ]',   $MATCH->{"square_parse"}->flat()  ] }

        | '{'  <.Perlito5::Grammar.ws>?
               <Perlito5::Grammar.exp_stmts> <.Perlito5::Grammar.ws>? '}'
                    { $MATCH->{"capture"} = [ 'postfix_or_term', 'block', $MATCH->{"Perlito5::Grammar.exp_stmts"}->flat() ] }
        | 'sub' <.Perlito5::Grammar.ws> <Perlito5::Grammar.sub_def>
                    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.sub_def"}->flat()     ] }

        # XXX Perl6
        | 'token' <.Perlito5::Grammar.ws> <Perlito5::Grammar.token>
                    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.token"}->flat()       ] }
        | 'do' <.Perlito5::Grammar.ws> <statement_parse>
                    { $MATCH->{"capture"} = [ 'term', Do->new( block => $MATCH->{"statement_parse"}->flat() ) ] }

        | '?'  <ternary5_parse> ':'
                    { $MATCH->{"capture"} = [ 'op',          '?? !!', $MATCH->{"ternary5_parse"}->flat()  ] }
        | <Perlito5::Grammar.var_ident>                { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.var_ident"}->flat()   ] }

        | '*' <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.var_name> <before <.Perlito5::Grammar.ws> '=' >
            {
                $MATCH->{"capture"} = [
                    'term', 
                    Var->new(
                        sigil       => '*',
                        namespace   => $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat(),
                        name        => $MATCH->{"Perlito5::Grammar.var_name"}->flat(),
                    )
                ]
            }

        | <Perlito5::Precedence.op_parse>              { $MATCH->{"capture"} = $MATCH->{"Perlito5::Precedence.op_parse"}->flat()             }

        | 'use'   <.Perlito5::Grammar.ws> <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <list_parse>
            { $MATCH->{"capture"} = [ 'term', Use->new( mod => $MATCH->{"Perlito5::Grammar.full_ident"}->flat() ) ] }

        | 'package' <.Perlito5::Grammar.ws> <Perlito5::Grammar.full_ident>
            { $MATCH->{"capture"} = [ 'term',
                     Apply->new(
                        code => 'package', arguments => [], namespace => $MATCH->{"Perlito5::Grammar.full_ident"}->flat() 
                     )
                   ]
            }

        | <Perlito5::Grammar.declarator> <.Perlito5::Grammar.ws> <Perlito5::Grammar.opt_type> <.Perlito5::Grammar.opt_ws> <Perlito5::Grammar.var_ident>   # my Int $variable
            { $MATCH->{"capture"} = [ 'term', Decl->new( decl => $MATCH->{"Perlito5::Grammar.declarator"}->flat(), type => $MATCH->{"Perlito5::Grammar.opt_type"}->flat(), var => $MATCH->{"Perlito5::Grammar.var_ident"}->flat() ) ] }

        | '->' 
            <Perlito5::Grammar.ident>
          [ ':' <.Perlito5::Grammar.ws>? <list_parse>
            { $MATCH->{"capture"} = [ 'postfix_or_term', 'methcall', $MATCH->{"Perlito5::Grammar.ident"}->flat(), $MATCH->{"list_parse"}->flat() ] }
          | '(' <paren_parse> ')'
            { $MATCH->{"capture"} = [ 'postfix_or_term',
                     'methcall',
                     $MATCH->{"Perlito5::Grammar.ident"}->flat(),
                     { end_block => undef,
                       exp       => $MATCH->{"paren_parse"}->flat(),
                       terminated => 0,
                     },
                   ]
            }
          | { $MATCH->{"capture"} = [ 'postfix_or_term', 'methcall_no_params', $MATCH->{"Perlito5::Grammar.ident"}->flat() ] }
          ]

        | <before <.Perlito5::Grammar.digit> >
            [ <Perlito5::Grammar.val_num>    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.val_num"}->flat() ]  }  # 123.456
            | <Perlito5::Grammar.val_int>    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.val_int"}->flat() ]  }  # 123
            ]

        | <before <.Perlito5::Grammar.word> >
          <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.ident>

          [
            <before <.Perlito5::Grammar.ws>? '=>' >   # autoquote
            { my $namespace = $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat();
              my $name      = $MATCH->{"Perlito5::Grammar.ident"}->flat();
              if ($namespace) {
                $name = $namespace . '::' . $name;
              }
              $MATCH->{"capture"} = [ 'term', Val::Buf->new( buf => $name ) ] 
            }

          | <.Perlito5::Grammar.ws> <list_parse>
            { $MATCH->{"capture"} = [ 'postfix_or_term', 'funcall',
                     $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat(),
                     $MATCH->{"Perlito5::Grammar.ident"}->flat(), 
                     $MATCH->{"list_parse"}->flat()  
                   ] 
            }
          | <before '->' >
            { my $namespace = $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat();
              my $name      = $MATCH->{"Perlito5::Grammar.ident"}->flat();
              if ($namespace) {
                $name = $namespace . '::' . $name;
              }
              $MATCH->{"capture"} = [ 'term', Proto->new( name => $name )            ]
            }
          | { $MATCH->{"capture"} = [ 'postfix_or_term', 'funcall_no_params',
                     $MATCH->{"Perlito5::Grammar.optional_namespace_before_ident"}->flat(),
                     $MATCH->{"Perlito5::Grammar.ident"}->flat()                  ] }
          ]

        | <Perlito5::Grammar.val_buf>    { $MATCH->{"capture"} = [ 'term', $MATCH->{"Perlito5::Grammar.val_buf"}->flat() ]  }  # 'moose'

        | <.Perlito5::Grammar.ws>                      { $MATCH->{"capture"} = [ 'space',   ' '                             ] }
    }

    token has_newline_after {
        |    '#'
        |    <.Perlito5::Grammar.is_newline>
        |    <.Perlito5::Grammar.space>  <.has_newline_after>
    }
    token has_no_comma_or_colon_after {
        <.Perlito5::Grammar.ws> <!before [ ',' | ':' ]> .
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
                my $m = $self->operator($str, $last_pos);
                # say "# list lexer got: " . $m->perl;
                if (!$m->bool) {
                    return [ 'end', '*end*' ];
                }
                $v = $m->flat();
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
        my $prec = Perlito5::Precedence->new(get_token => $get_token, reduce => $reduce_to_ast,
            end_token => [ 'and', 'or', ':', ']', ')', '}', ';', 
                           'if', 'else', 'elsif', 'unless', 'when', 'foreach', 'for', 'while'
                         ] 
        );
        my $res = $prec->precedence_parse;
        # say "# list_lexer return: ", $res->perl;
        if (scalar(@$res) == 0) {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
                capture => {
                    exp        => '*undef*',
                    end_block  => undef,
                    terminated => undef } )
        }
        # if the expression terminates in a block, the block was pushed to num_stack
        my $block;
        if (scalar(@$res) > 1) {
            $block = pop @$res; # pop_term($res);
            $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            # say "# list exp terminated with a block: ", $block->perl;
        }
        my $result = pop_term($res);
        if (scalar(@$res) > 0) {
            $block = pop @$res; # pop_term($res);
            $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            # say "# list exp terminated with a block (2): ", $block->perl;
        }
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
            capture => {
                exp        => $result,
                end_block  => $block,
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
        my $get_token = sub {
            my $m = $self->operator($str, $last_pos);
            if (!$m->bool) {
                die "Expected closing delimiter: ", @{$delimiter}, ' near ', $last_pos;
            }
            my $v = $m->flat();
            if ($v->[0] ne 'end') {
                $last_pos = $m->to;
            }
            # say "# circumfix_lexer " . $v->perl;
            return $v;
        };
        my $prec = Perlito5::Precedence->new(get_token => $get_token, reduce => $reduce_to_ast,
            end_token => $delimiter );
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
       
        return $self->circumfix_parse($str, $pos, [':']);
    }
    sub curly_parse {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];
       
        return $self->circumfix_parse($str, $pos, ['}']);
    }
    sub square_parse {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];
       
        return $self->circumfix_parse($str, $pos, [']']);
    }
    sub paren_parse {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];
       
        return $self->circumfix_parse($str, $pos, [')']);
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
        my $get_token = sub {
            my $v;
            if (scalar(@$lexer_stack)) {
                $v = pop @$lexer_stack;
            }
            else {
                my $m = $self->operator($str, $last_pos);
                # say "# exp lexer got: " . $m->perl;
                if (!$m->bool) {
                    return [ 'end', '*end*' ];
                }
                $v = $m->flat();
                if ($v->[0] ne 'end') {
                    $last_pos = $m->to;
                }
            }
            # say "# exp_lexer got " . $v->perl;

            if  (  ( $v->[0] eq 'postfix_or_term' && $v->[1] eq 'block' )
                || ( $v->[0] eq 'term' && ref($v->[1]) eq 'Sub' )
                || ( $v->[0] eq 'term' && ref($v->[1]) eq 'Do' )
                || ( $v->[0] eq 'term' && ref($v->[1]) eq 'CompUnit' )
                )
            {
                # a block followed by newline terminates the expression
                if ($self->has_newline_after($str, $last_pos)->bool) {
                    $terminated = 1;
                    push( @$lexer_stack,  [ 'end', '*end*' ] );
                }
            }

            return $v;
        };
        my $prec = Perlito5::Precedence->new(get_token => $get_token, reduce => $reduce_to_ast,
            end_token => [ ']', ')', '}', ';', 
                           'if', 'else', 'elsif', 'unless', 'when', 'foreach', 'for', 'while'
                         ] 
        );
        my $res = $prec->precedence_parse;
        # say "# exp terminated";
        if (scalar(@$res) == 0) {
            # say "# exp terminated with false";
            return Perlito5::Match->new(bool => 0);
        }
        # if the expression terminates in a block, the block was pushed to num_stack
        my $block;
        if (scalar(@$res) > 1) {
            $block = pop @$res; # pop_term($res);
            $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            # say "# exp terminated with a block: ", $block->perl;
        }
        my $result = pop_term($res);
        if (scalar(@$res) > 0) {
            $block = pop @$res; # pop_term($res);
            if (!( ref($block) eq 'Lit::Block' )) {
                $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            }
            # say "# exp terminated with a block (2): ", $block->perl;
        }
        # say "# exp_parse result: ", $result->perl;
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
            capture => {
                exp        => $result,
                end_block  => $block,
                terminated => $terminated } )
    }

    token exp_stmt {
        | <Perlito5::Grammar.if>     { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.if"}->flat()     }
        | <Perlito5::Grammar.unless> { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.unless"}->flat() }
        | <Perlito5::Grammar.when>   { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.when"}->flat()   }
        | <Perlito5::Grammar.for>    { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.for"}->flat()    }
        | <Perlito5::Grammar.while>  { $MATCH->{"capture"} = $MATCH->{"Perlito5::Grammar.while"}->flat()  }
    }

    token statement_modifier {
        'if' | 'unless' | 'when' | 'foreach' | 'for' | 'while'
    }

    token delimited_statement {
        <.Perlito5::Grammar.ws>?
        [ ';' <.Perlito5::Grammar.ws>?
        | <statement_parse> ';'? <.Perlito5::Grammar.ws>?
            { $MATCH->{"capture"} = $MATCH->{"statement_parse"}->flat() }
        ]
    }

    sub statement_parse {
        my $self = $_[0];
        my $str = $_[1];
        my $pos = $_[2];
       
        # say "# statement_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $lexer_stack = [];
        my $res;
        $res = $self->exp_stmt($str, $pos);
        if ($res->bool) {
            # say "# statement result: ", $res->perl;
            return $res;
        }
        $res = $self->exp_parse($str, $pos);
        if (!$res->bool) {
            # say "# not a statement or expression";
            return $res;
        }
        if ( ref( $res->flat()->{'exp'} ) eq 'Lit::Block' ) {
            # standalone block
            $res->flat()->{'exp'} = Do->new(block => $res->flat()->{'exp'});
        }
        if ($res->flat()->{'end_block'}) {
            # warn "Block: ", $res->flat()->{'end_block'}->perl;
            die "Unexpected block after expression near ", $pos;
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
        if ($modifier_exp->flat()->{'end_block'}) {
            # warn "Block: ", $modifier_exp->flat()->{'end_block'}->perl;
            die "Unexpected block after expression near ", $modifier->to;
        }
        # TODO - require a statement terminator
        # say "# statement_parse modifier result: ", $modifier_exp->perl;

        $modifier = $modifier->flat();

        if ($modifier eq 'if') {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => If->new(
                    cond      => $modifier_exp->flat()->{'exp'},
                    body      => Lit::Block->new(stmts => [ $res->flat()->{'exp'} ]),
                    otherwise => Lit::Block->new(stmts => [ ]) ) );
        }
        if ($modifier eq 'unless') {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => If->new(
                    cond      => $modifier_exp->flat()->{'exp'},
                    body      => Lit::Block->new(stmts => [ ]),
                    otherwise => Lit::Block->new(stmts => [ $res->flat()->{'exp'} ]) ) );
        }
        if ($modifier eq 'while') {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => While->new(
                    cond    => $modifier_exp->flat()->{'exp'},
                    body    => Lit::Block->new(stmts => [ $res->flat()->{'exp'} ] ) ) );
        }
        if  (  $modifier eq 'for'
            || $modifier eq 'foreach'
            ) 
        {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => For->new(
                    cond    => $modifier_exp->flat()->{'exp'},
                    body    => Lit::Block->new(stmts => [ $res->flat()->{'exp'} ] ) ) );
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

This module parses source code for Perl 6 statements and generates Perlito AST.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


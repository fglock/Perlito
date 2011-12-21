
class Perlito5::Expression {
    use Perlito5::Precedence;
    use Perlito5::Grammar;
    use Perlito5::Perl5::Emitter;

    sub expand_list {
        my $param_list = shift;
        # say "# expand_list: ", $param_list->perl;
        if ($param_list->isa('Apply')) && (($param_list->code) eq 'list:<,>') {
            my $args = [];
            for my $v ( @($param_list->arguments) ) {
                if defined($v) {
                    $args->push($v);
                }
            }
            return $args;
        }
        elsif $param_list eq '*undef*' {
            return [];
        }
        else {
            return [ $param_list ];
        }
    }

    sub block_or_hash {
        my $o = shift;
        # say "# block_or_hash? ", $o->perl;
        if defined($o->sig) {
            # say "#  has sig -- not a block";
            return $o
        }
        my $stmts = $o->stmts;
        if (!(defined $stmts)) || (($stmts->elems) == 0) {
            # say "#  no contents -- empty hash";
            return Lit::Hash->new(hash1 => [])
        }
        if ($stmts->elems) != 1 {
            # say "#  more statements -- not hash";
            return $o
        }
        my $stmt = $stmts->[0];
        if !($stmt->isa('Apply')) {
            # say "#  not Apply -- not hash";
            return $o
        }
        if ($stmt->code) eq 'infix:<=>>' {
            # the argument is a single pair
            # say "#  single pair -- is hash";
            return Lit::Hash->new(hash1 => [ $stmt ])
        }
        if ($stmt->code) ne 'list:<,>' {
            # say "#  not a list -- not hash";
            return $o
        }
        # the argument is a list -- check that it contains a pair
        for my $item ( @($stmt->arguments) ) {
            # say "#  item: ", $item->perl;
            if $item->isa('Apply') && ($item->code) eq 'infix:<=>>' {
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
        my $v = $num_stack->pop;
        if $v->isa('Array') {
            # say "# ** processing term ", $v->perl;
            if $v->[1] eq 'methcall_no_params' {
                # say "#   Call ", ($v->[2])->perl;
                $v = Call->new( invocant => Mu, method => $v->[2], hyper => $v->[3] );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq 'funcall_no_params' {
                # say "#   Apply ", ($v->[2])->perl;
                $v = Apply->new( code => $v->[3], namespace => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq 'methcall' {
                # say "#   Call ", ($v->[2])->perl;
                if ($v->[3])<end_block> {
                    # say "# pop_term: found end_block in Call";
                    $num_stack->unshift( ($v->[3])<end_block> );
                }
                my $param_list = expand_list( ($v->[3])<exp> );
                $v = Call->new( invocant => Mu, method => $v->[2], arguments => $param_list, hyper => $v->[4] );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq 'funcall' {
                # say "#   Apply ", ($v->[2])->perl;
                if ($v->[4])<end_block> {
                    # say "# pop_term: found end_block in Apply";
                    $num_stack->unshift( ($v->[4])<end_block> );
                }
                my $param_list = expand_list( ($v->[4])<exp> );
                $v = Apply->new( code => $v->[3], arguments => $param_list, namespace => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq '( )' {
                # say "#   Plain parentheses ", ($v->[2])->perl;
                my $param_list = expand_list($v->[2]);
                $v = Apply->new( code => 'circumfix:<( )>', arguments => $param_list, namespace => '' );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq '[ ]' {
                # say "#   Array ", ($v->[2])->perl;
                my $param_list = expand_list($v->[2]);
                $v = Lit::Array->new( array1 => $param_list );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq 'block' {
                # say "#   Block, Hash, or Pair ", ($v->[2])->perl;
                $v = Lit::Block->new( stmts => $v->[2], sig => $v->[3] );
                $v = block_or_hash($v);
                # TODO: $v = Lit::Hash->new( hash1 => $v->[2] );
                return $v;
            }
            if $v->[1] eq '.( )' {
                # say "#   Params ", ($v->[2])->perl;
                # say "#     v:     ", $v->perl;
                $v = Call->new( invocant => Mu, method => 'postcircumfix:<( )>', arguments => $v->[2], hyper => 0 );
                return $v;
            }
            if $v->[1] eq '.[ ]' {
                # say "#   Index ", ($v->[2])->perl;
                $v = Index->new( obj => Mu, index_exp => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if $v->[1] eq '.{ }' {
                # say "#   Lookup ", ($v->[2])->perl;
                $v = Lookup->new( obj => Mu, index_exp => $v->[2] );
                # say "#     ", $v->perl;
                return $v;
            }
            if ($v->[1])->isa('Array') && ((($v->[1])->elems) == 2) {
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
        if $v->[1] eq 'methcall_no_params' {
            # say "#   Call ", ($v->[2])->perl;
            $v = Call->new( invocant => $value, method => $v->[2], hyper => $v->[3] );
            return $v;
        }
        if $v->[1] eq 'funcall_no_params' {
            die "unexpected function call";
            # say "#   Apply ", ($v->[2])->perl;
            push $v, $value;
            return $v;
        }
        if $v->[1] eq 'methcall' {
            # say "#   Call ", ($v->[2])->perl;
            my $param_list = expand_list(($v->[3])<exp>);
            $v = Call->new( invocant => $value, method => $v->[2], arguments => $param_list, hyper => $v->[4] );
            return $v;
        }
        if $v->[1] eq 'funcall' {
            die "unexpected function call";
            # say "#   Apply ", ($v->[2])->perl;
            push $v, $value;
            return $v;
        }
        if $v->[1] eq '( )' {
            # say "#   Params ", ($v->[2])->perl;
            my $param_list = expand_list($v->[2]);
            if $value->isa('Apply') && !(defined($value->arguments)) {
                $value->arguments = $param_list;
                return $value;
            }
            if $value->isa('Call') && !(defined($value->arguments)) {
                $value->arguments = $param_list;
                return $value;
            }
            $v = Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list, hyper => 0 );
            return $v;
        }
        if $v->[1] eq '[ ]' {
            # say "#   Index ", ($v->[2])->perl;
            $v = Index->new( obj => $value, index_exp => $v->[2] );
            # say "#     ", $v->perl;
            return $v;
        }
        if $v->[1] eq 'block' {
            # say "#   Lookup (was Block)", ($v->[2])->perl;
            $v = Lookup->new( obj => $value, index_exp => ($v->[2])[0] );
            return $v;
        }
        if $v->[1] eq '.( )' {
            my $param_list = expand_list($v->[2]);
            $v = Call->new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $param_list, hyper => 0 );
            return $v;
        }
        if $v->[1] eq '.[ ]' {
            $v = Call->new( invocant => $value, method => 'postcircumfix:<[ ]>', arguments => $v->[2], hyper => 0 );
            return $v;
        }
        if $v->[1] eq '.{ }' {
            $v = Call->new( invocant => $value, method => 'postcircumfix:<{ }>', arguments => $v->[2], hyper => 0 );
            return $v;
        }
        push $op, $value;
        return $op;
    }

    my $reduce_to_ast = sub ($op_stack, $num_stack) {
        my $last_op = $op_stack->shift;
        # say "# reduce_to_ast ";
        # say "#     last_op: ", $last_op->perl;
        # say "#   num_stack: ", $num_stack;
        if $last_op->[0] eq 'prefix' {
            push $num_stack,
                Apply->new(
                    namespace => '',
                    code      => 'prefix:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif $last_op->[0] eq 'postfix' {
            push $num_stack,
                Apply->new(
                    namespace => '',
                    code      => 'postfix:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif $last_op->[0] eq 'postfix_or_term' {
            $num_stack->push( reduce_postfix( $last_op, pop_term($num_stack) ) );
        }
        elsif Perlito5::Precedence::is_assoc_type('list', $last_op->[1]) {
            my $arg;
            if $num_stack->elems < 2 {
                my $v2 = pop_term($num_stack);
                if ($v2->isa('Apply')) && ($v2->code eq ('list:<' . $last_op->[1] . '>')) {
                    ($v2->arguments)->push( Mu );
                    $num_stack->push( $v2 );
                }
                else {
                    push $num_stack,
                        Apply->new(
                            namespace => '',
                            code      => 'list:<' . $last_op->[1] . '>',
                            arguments => [ $v2, Mu ],
                          );
                }
                return;
            }
            else {
                my $v2 = pop_term($num_stack);
                $arg = [ pop_term($num_stack), $v2 ];
            }
            if     (($arg->[0])->isa('Apply'))
                && ($last_op->[0] eq 'infix')
                && (($arg->[0])->code eq ('list:<' . $last_op->[1] . '>'))
            {
                push $num_stack,
                    Apply->new(
                        namespace => '',
                        code      => ($arg->[0])->code,
                        arguments => [ @( ($arg->[0])->arguments ), $arg->[1] ],
                      );
                return;
            }
            push $num_stack,
                Apply->new(
                    namespace => '',
                    code      => 'list:<' . $last_op->[1] . '>',
                    arguments => $arg,
                  );
        }
        elsif Perlito5::Precedence::is_assoc_type('chain', $last_op->[1]) {
            if $num_stack->elems < 2 {
                die("Missing value after operator " . $last_op->[1]);
            }
            my $v2 = pop_term($num_stack);
            my $arg = [ pop_term($num_stack), $v2 ];
            # say "# assoc chain: ", $arg->perl;

            # TODO - create a special AST node for assoc chain?
            # if ($arg->[0])->isa('Apply')
            #     && Perlito5::Precedence::is_assoc_type('chain', ($arg->[1]){op} )
            # {
            #     push $num_stack,
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
            push $num_stack,
                    Apply->new(
                        namespace => '',
                        code      => 'infix:<' . $last_op->[1] . '>',
                        arguments => $arg
                    );
        }
        elsif $last_op->[0] eq 'ternary' {
            if ( $num_stack->elems < 2 ) {
                die "Missing value after ternary operator";
            }
            my $v2 = pop_term($num_stack);
            push $num_stack,
                Apply->new(
                    namespace => '',
                    code      => 'ternary:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack), $last_op->[2], $v2 ],
                  );
        }
        else {
            if ( $num_stack->elems < 2 ) {
                die("missing value after operator '" . $last_op->[1] . "'");
            }
            my $v2 = pop_term($num_stack);
            push $num_stack,
                Apply->new(
                    namespace => '',
                    code      => 'infix:<' . $last_op->[1] . '>',
                    arguments => [ pop_term($num_stack), $v2 ],
                  );
        }
    };

    # TODO
    # token pair {
    #     |   \: <var_sigil> <ident>                  #  :$var
    #                 Val::Buf->new( buf => ~$<ident> ),
    #                 Var->new( sigil => ~$$<var_sigil>, twigil => '', name => $$<ident> ) ]

    token capture_name {
        <Perlito5::Grammar.full_ident> [ [ '.' | '->' ] <Perlito5::Grammar.ident> ]?
    }

    token hyper_op {
        '>>'?
    }

    token operator {
        # XXX Perl6
        | '.(' <paren_parse>   ')'                      { make [ 'postfix_or_term',  '.( )',  $$<paren_parse>   ] }
        | '.[' <square_parse>  ']'                      { make [ 'postfix_or_term',  '.[ ]',  $$<square_parse>  ] }
        | '.{' <curly_parse>   '}'                      { make [ 'postfix_or_term',  'block', $$<curly_parse>   ] }

        | '->(' <paren_parse>   ')'                     { make [ 'postfix_or_term',  '.( )',  $$<paren_parse>   ] }
        | '->[' <square_parse>  ']'                     { make [ 'postfix_or_term',  '.[ ]',  $$<square_parse>  ] }
        | '->{' <curly_parse>   '}'                     { make [ 'postfix_or_term',  '.{ }',  $$<curly_parse>   ] }
        | '('  <paren_parse>   ')'                      { make [ 'postfix_or_term',  '( )',   $$<paren_parse>   ] }
        | '['  <square_parse>  ']'                      { make [ 'postfix_or_term',  '[ ]',   $$<square_parse>  ] }
        | [ '.<' | '<' ] <Perlito5::Grammar.ident> '>'   { make [ 'postfix_or_term',  'block', [Val::Buf->new('buf' => $$<Perlito5::Grammar.ident>)] ] }

        | '{'  <.Perlito5::Grammar.ws>?
               <Perlito5::Grammar.exp_stmts> <.Perlito5::Grammar.ws>? '}'
                    { make [ 'postfix_or_term', 'block', $$<Perlito5::Grammar.exp_stmts> ] }
        | 'method' <.Perlito5::Grammar.ws> <Perlito5::Grammar.method_def>
                    { make [ 'term', $$<Perlito5::Grammar.method_def>  ] }
        | 'sub' <.Perlito5::Grammar.ws> <Perlito5::Grammar.sub_def>
                    { make [ 'term', $$<Perlito5::Grammar.sub_def>     ] }
        | 'token' <.Perlito5::Grammar.ws> <Perlito5::Grammar.token>
                    { make [ 'term', $$<Perlito5::Grammar.token>       ] }
        | 'do' <.Perlito5::Grammar.ws> <statement_parse>
                    { make [ 'term', Do->new( block => $$<statement_parse> ) ] }

        | '?'  <ternary5_parse> ':'
                    { make [ 'op',          '?? !!', $$<ternary5_parse>  ] }
        | <Perlito5::Grammar.var_ident>                { make [ 'term', $$<Perlito5::Grammar.var_ident>   ] }
        | '$<' <capture_name> '>'
            { make [ 'term', Lookup->new(
                obj   => Var->new( sigil => '$', twigil => '', name => '/' ),
                index_exp => Val::Buf->new( buf => ~$<capture_name> )
            ) ] }
        | <Perlito5::Precedence.op_parse>              { make $$<Perlito5::Precedence.op_parse>             }
        | <Perlito5::Grammar.ident> <before <.Perlito5::Grammar.ws>? '=>' >   # autoquote
            { make [ 'term', Val::Buf->new( buf => ~$<Perlito5::Grammar.ident> ) ] }
        | 'True'  <!before [ <.Perlito5::Grammar.word> | '(' ] > { make [ 'term', Val::Bit->new( bit => 1 ) ] }
        | 'False' <!before [ <.Perlito5::Grammar.word> | '(' ] > { make [ 'term', Val::Bit->new( bit => 0 ) ] }
        | 'and'   <!before [ <.Perlito5::Grammar.word> | '(' ] > { make [ 'op',    'and'                   ] }
        | 'not'   <!before [ <.Perlito5::Grammar.word> | '(' ] > { make [ 'op',    'not'                   ] }
        | 'use'   <.Perlito5::Grammar.ws> <Perlito5::Grammar.full_ident>  [ - <Perlito5::Grammar.ident> ]? <list_parse>
            { make [ 'term', Use->new( mod => $$<Perlito5::Grammar.full_ident> ) ] }

          # XXX Perl6
        | 'class' <.Perlito5::Grammar.ws> <Perlito5::Grammar.grammar>
            { make [ 'term', $$<Perlito5::Grammar.grammar> ] }

        | 'package' <.Perlito5::Grammar.ws> <Perlito5::Grammar.full_ident>
            { make [ 'term',
                     Apply->new(
                        code => 'package', arguments => [], namespace => $$<Perlito5::Grammar.full_ident> 
                     )
                   ]
            }

        | <Perlito5::Grammar.declarator> <.Perlito5::Grammar.ws> <Perlito5::Grammar.opt_type> <.Perlito5::Grammar.opt_ws> <Perlito5::Grammar.var_ident>   # my Int $variable
            { make [ 'term', Decl->new( decl => $$<Perlito5::Grammar.declarator>, type => $$<Perlito5::Grammar.opt_type>, var => $$<Perlito5::Grammar.var_ident> ) ] }

        | [ '.' | '->' ] <hyper_op> <Perlito5::Grammar.ident>
          [ ':' <.Perlito5::Grammar.ws>? <list_parse>
            { make [ 'postfix_or_term', 'methcall',           ~$<Perlito5::Grammar.ident>, $$<list_parse>, $$<hyper_op>  ] }
          | '(' <paren_parse> ')'
            { make [ 'postfix_or_term',
                     'methcall',
                     ~$<Perlito5::Grammar.ident>,
                     { end_block => Mu,
                       exp       => $$<paren_parse>,
                       terminated => 0,
                     },
                     $$<hyper_op>
                   ]
            }
          | { make [ 'postfix_or_term', 'methcall_no_params', ~$<Perlito5::Grammar.ident>, $$<hyper_op>                  ] }
          ]

        | <Perlito5::Grammar.optional_namespace_before_ident> <Perlito5::Grammar.ident>
          [ <.Perlito5::Grammar.ws> <list_parse>
            { make [ 'postfix_or_term', 'funcall',
                     ~$<Perlito5::Grammar.optional_namespace_before_ident>,
                     ~$<Perlito5::Grammar.ident>, $$<list_parse>  ] }
          | <before [ '.' | '->' ] >
            { my $namespace = ~$<Perlito5::Grammar.optional_namespace_before_ident>;
              my $name      = ~$<Perlito5::Grammar.ident>;
              if $namespace {
                $name = $namespace . '::' . $name;
              }
              make [ 'term', Proto->new( name => $name )            ]
            }
          | { make [ 'postfix_or_term', 'funcall_no_params',
                     ~$<Perlito5::Grammar.optional_namespace_before_ident>,
                     ~$<Perlito5::Grammar.ident>                  ] }
          ]
        | <Perlito5::Grammar.val_num>    { make [ 'term', $$<Perlito5::Grammar.val_num> ]  }  # 123.456
        | <Perlito5::Grammar.val_int>    { make [ 'term', $$<Perlito5::Grammar.val_int> ]  }  # 123
        | <Perlito5::Grammar.val_buf>    { make [ 'term', $$<Perlito5::Grammar.val_buf> ]  }  # 'moose'

        | <.Perlito5::Grammar.ws>                      { make [ 'space',   ' '                             ] }
    }

    token has_newline_after {
        |    '#'
        |    <.Perlito5::Grammar.is_newline>
        |    <.Perlito5::Grammar.space>  <.has_newline_after>
    }
    token has_no_comma_or_colon_after {
        <.Perlito5::Grammar.ws> <!before [ ',' | ':' ]> .
    }

    method list_parse ($str, $pos) {
        # say "# list_parse: input ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $is_first_token = True;
        my $lexer_stack = [];
        my $terminated = 0;
        my $last_token_was_space = 1;
        my $get_token = sub {
            my $v;
            if $lexer_stack->elems() {
                $v = $lexer_stack->pop;
                if  $is_first_token
                    && ($v->[0] eq 'op')
                    && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                {
                    # say "# finishing list - first token is: ", $v->[1];
                    $v->[0] = 'end';
                }
            }
            else {
                my $m = self->operator($str, $last_pos);
                # say "# list lexer got: " . $m->perl;
                if !$m {
                    return [ 'end', '*end*' ];
                }
                $v = $$m;
                if  $is_first_token
                    && ($v->[0] eq 'op')
                    && !(Perlito5::Precedence::is_fixity_type('prefix', $v->[1]))
                {
                    # say "# finishing list - first token is: ", $v->[1];
                    $v->[0] = 'end';
                }
                if $v->[0] ne 'end' {
                    $last_pos = $m->to;
                }
            }
            # say "# list_lexer got " . $v->perl;

            # say "# list_lexer " . $v->perl;

            if (($v->[0]) eq 'postfix_or_term') && (($v->[1]) eq 'block')
                && $last_token_was_space
            {
                if self->has_newline_after($str, $last_pos) {
                    # a block followed by newline terminates the expression
                    $terminated = 1;
                    $lexer_stack->push( [ 'end', '*end*' ] );
                }
                elsif self->has_no_comma_or_colon_after($str, $last_pos) {
                    # a sequence ( block - space - not_comma_or_colon ) terminates the list
                    $terminated = 1;
                    $lexer_stack->push( [ 'end', '*end*' ] );
                }
            }
            $last_token_was_space = ($v->[0] eq 'space');
            $is_first_token = False;

            return $v;
        };
        my $prec = Perlito5::Precedence->new(get_token => $get_token, reduce => $reduce_to_ast,
            end_token => [ 'and', 'or', ':', ']', ')', '}', ';', 'if', 'else', 'elsif', 'unless', 'when', 'for', 'while', 'loop' ] );
        my $res = $prec->precedence_parse;
        # say "# list_lexer return: ", $res->perl;
        if $res->elems == 0 {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1,
                capture => {
                    exp        => '*undef*',
                    end_block  => Mu,
                    terminated => Mu } )
        }
        # if the expression terminates in a block, the block was pushed to num_stack
        my $block;
        if $res->elems > 1 {
            $block = $res->pop; # pop_term($res);
            $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            # say "# list exp terminated with a block: ", $block->perl;
        }
        my $result = pop_term($res);
        if $res->elems > 0 {
            $block = $res->pop; # pop_term($res);
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

    method circumfix_parse ($str, $pos, $delimiter) {
        # say "# circumfix_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self->operator($str, $last_pos);
            if !$m {
                die "Expected closing delimiter: ", @($delimiter), ' near ', $last_pos;
            }
            my $v = $$m;
            if $v->[0] ne 'end' {
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
        if !(defined($res)) {
            # can't return undef in a capture (BUG in Match object?)
            $res = '*undef*';
        }
        return Perlito5::Match->new(
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }

    method ternary5_parse ($str, $pos) {
        return self->circumfix_parse($str, $pos, [':']);
    }
    method curly_parse ($str, $pos) {
        return self->circumfix_parse($str, $pos, ['}']);
    }
    method square_parse ($str, $pos) {
        return self->circumfix_parse($str, $pos, [']']);
    }
    method paren_parse ($str, $pos) {
        return self->circumfix_parse($str, $pos, [')']);
    }

    method exp_parse ($str, $pos) {
        # say "# exp_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $lexer_stack = [];
        my $terminated = 0;
        my $get_token = sub {
            my $v;
            if $lexer_stack->elems() {
                $v = $lexer_stack->pop;
            }
            else {
                my $m = self->operator($str, $last_pos);
                # say "# exp lexer got: " . $m->perl;
                if !$m {
                    return [ 'end', '*end*' ];
                }
                $v = $$m;
                if $v->[0] ne 'end' {
                    $last_pos = $m->to;
                }
            }
            # say "# exp_lexer got " . $v->perl;

            if     ( (($v->[0]) eq 'postfix_or_term') && (($v->[1]) eq 'block') )
                || ( (($v->[0]) eq 'term') && (($v->[1])->isa('Sub')) )
                || ( (($v->[0]) eq 'term') && (($v->[1])->isa('Method')) )
                || ( (($v->[0]) eq 'term') && (($v->[1])->isa('Do')) )
                || ( (($v->[0]) eq 'term') && (($v->[1])->isa('CompUnit')) )
            {
                # a block followed by newline terminates the expression
                if self->has_newline_after($str, $last_pos) {
                    $terminated = 1;
                    $lexer_stack->push( [ 'end', '*end*' ] );
                }
            }

            return $v;
        };
        my $prec = Perlito5::Precedence->new(get_token => $get_token, reduce => $reduce_to_ast,
            end_token => [ ']', ')', '}', ';', 'if', 'else', 'elsif', 'unless', 'when', 'for', 'while', 'loop' ] );
        my $res = $prec->precedence_parse;
        # say "# exp terminated";
        if $res->elems == 0 {
            # say "# exp terminated with false";
            return Perlito5::Match->new(bool => 0);
        }
        # if the expression terminates in a block, the block was pushed to num_stack
        my $block;
        if $res->elems > 1 {
            $block = $res->pop; # pop_term($res);
            $block = Lit::Block->new( stmts => $block->[2], sig => $block->[3] );
            # say "# exp terminated with a block: ", $block->perl;
        }
        my $result = pop_term($res);
        if $res->elems > 0 {
            $block = $res->pop; # pop_term($res);
            if !($block->isa('Lit::Block')) {
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
        | <Perlito5::Grammar.if>     { make $$<Perlito5::Grammar.if>     }
        | <Perlito5::Grammar.unless> { make $$<Perlito5::Grammar.unless> }
        | <Perlito5::Grammar.when>   { make $$<Perlito5::Grammar.when>   }
        | <Perlito5::Grammar.for>    { make $$<Perlito5::Grammar.for>    }
        | <Perlito5::Grammar.while>  { make $$<Perlito5::Grammar.while>  }
        | <Perlito5::Grammar.loop>   { make $$<Perlito5::Grammar.loop>   }
    }

    token statement_modifier {
        'if' | 'unless' | 'when' | 'for' | 'while' | 'loop'
    }

    token delimited_statement {
        <.Perlito5::Grammar.ws>?
        [ ';' <.Perlito5::Grammar.ws>?
        | <statement_parse> ';'? <.Perlito5::Grammar.ws>?
            { make $$<statement_parse> }
        ]
    }

    token delimited_statement_no_package {
        <.Perlito5::Grammar.ws>?
        [ ';' <.Perlito5::Grammar.ws>?
        | <!before 'package' <.Perlito5::Grammar.ws> >
          <statement_parse> ';'? <.Perlito5::Grammar.ws>?
            { make $$<statement_parse> }
        ]
    }

    method statement_parse ($str, $pos) {
        # say "# statement_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $lexer_stack = [];
        my $res;
        $res = self->exp_stmt($str, $pos);
        if $res {
            # say "# statement result: ", $res->perl;
            return $res;
        }
        $res = self->exp_parse($str, $pos);
        if !($res) {
            # say "# not a statement or expression";
            return $res;
        }
        if ($$res)<exp>->isa('Lit::Block') {
            # standalone block
            ($$res)<exp> = Do->new(block => ($$res)<exp>);
        }
        if ($$res)<end_block> {
            # warn "Block: ", (($$res)<end_block>)->perl;
            die "Unexpected block after expression near ", $pos;
        }
        if ($$res)<terminated> {
            # say "# statement expression terminated result: ", $res->perl;
            $res->capture = ($$res)<exp>;
            return $res;
        }
        # say "# look for a statement modifier";
        my $modifier = self->statement_modifier($str, $res->to);
        if !($modifier) {
            # say "# statement expression no modifier result: ", $res->perl;
            # TODO - require a statement terminator
            $res->capture = ($$res)<exp>;
            return $res;
        }
        my $modifier_exp = self->exp_parse($str, $modifier->to);
        # say "# statement modifier [", $modifier, "] exp: ", $modifier_exp->perl;
        if !($modifier_exp) {
            die "Expected expression after '", $modifier, "'";
        }
        if ($$modifier_exp)<end_block> {
            # warn "Block: ", (($$modifier_exp)<end_block>)->perl;
            die "Unexpected block after expression near ", $modifier->to;
        }
        # TODO - require a statement terminator
        # say "# statement_parse modifier result: ", $modifier_exp->perl;

        $modifier = ~$modifier;

        if $modifier eq 'if' {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => If->new(
                    cond      => ($$modifier_exp)<exp>,
                    body      => Lit::Block->new(stmts => [ ($$res)<exp> ]),
                    otherwise => Lit::Block->new(stmts => [ ]) ) );
        }
        if $modifier eq 'unless' {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => If->new(
                    cond      => ($$modifier_exp)<exp>,
                    body      => Lit::Block->new(stmts => [ ]),
                    otherwise => Lit::Block->new(stmts => [ ($$res)<exp> ]) ) );
        }
        if $modifier eq 'while' {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => While->new(
                    cond    => ($$modifier_exp)<exp>,
                    body    => Lit::Block->new(stmts => [ ($$res)<exp> ] ) ) );
        }
        if $modifier eq 'for' {
            return Perlito5::Match->new(
                'str' => $str, 'from' => $pos, 'to' => $modifier_exp->to, 'bool' => 1,
                capture => For->new(
                    cond    => ($$modifier_exp)<exp>,
                    body    => Lit::Block->new(stmts => [ ($$res)<exp> ] ) ) );
        }
        die "Unexpected statement modifier '$modifier'";
    }

}

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



class MiniPerl6::Expression {
    use MiniPerl6::Precedence;
    use MiniPerl6::Grammar;
    use MiniPerl6::Perl5::Emitter;
   
    sub pop_term ($num_stack) {
        my $v = $num_stack.pop;
        if $v.isa('Array') {
            say "# ** processing term ", $v.perl;
            if $v[1] eq 'methcall_no_params' {
                say "#   Call ", ($v[2]).perl;
                $v = Call.new( invocant => $v[3], method => $v[2], arguments => undef, hyper => 0 );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq 'funcall_no_params' {
                say "#   Apply ", ($v[2]).perl;
                $v = Apply.new( code => $v[2], arguments => undef, namespace => '' );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq 'methcall' {
                say "#   Call ", ($v[2]).perl;
                $v = Call.new( invocant => undef, method => $v[2], arguments => $v[3], hyper => 0 );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq 'funcall' {
                say "#   Apply ", ($v[2]).perl;
                $v = Apply.new( code => $v[2], arguments => $v[3], namespace => '' );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq '( )' {
                say "#   Plain parentheses ", ($v[2]).perl;
                $v = Apply.new( code => 'circumfix:<( )>', arguments => $v[2], namespace => '' );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq '[ ]' {
                say "#   Array ", ($v[2]).perl;
                $v = Lit::Array.new( array1 => $v[2] );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq 'block' {
                say "#   Block", ($v[2]).perl;
                $v = Lit::Block.new( stmts => $v[2] );
                return $v;
            }
            if $v[1] eq 'hash' {
                say "#   Hash or Pair", ($v[2]).perl;
                $v = Lit::Hash.new( hash1 => $v[2] );
                return $v;
            }
            if $v[1] eq '.( )' {
                say "#   Params ", ($v[2]).perl;
                say "#     v:     ", $v.perl;
                $v = Call.new( invocant => undef, method => 'postcircumfix:<( )>', arguments => $v[2], hyper => 0 );
                return $v;
            }
            if $v[1] eq '.[ ]' {
                say "#   Index ", ($v[2]).perl;
                $v = Index.new( obj => undef, index_exp => $v[2] );
                say "#     ", $v.perl;
                return $v;
            }
            if $v[1] eq '.{ }' {
                say "#   Lookup ", ($v[2]).perl;
                $v = Lookup.new( obj => undef, index_exp => $v[2] );
                say "#     ", $v.perl;
                return $v;
            }
            if ($v[1]).isa('Array') && ((($v[1]).elems) == 2) {
                say "#   old style Pair ", $v.perl;
                #   old style Pair - wrap it into a subroutine for now
                $v = Apply.new( code => 'pair', arguments => $v[1], namespace => '' );
                say "#     ", $v.perl;
                return $v;
            }
            return $v[1];
        }
        return $v;
    }

    sub reduce_postfix ($op, $value) {
        my $v = $op;
        say "# ** reduce_postfix ", $op.perl;
        say "#      value: ", $value.perl;
        say "#      v:     ", $v.perl;
        if $v[1] eq 'methcall_no_params' {
            say "#   Call ", ($v[2]).perl;
            $v = Call.new( invocant => $value, method => $v[2], arguments => undef, hyper => 0 );
            return $v;
        }
        if $v[1] eq 'funcall_no_params' {
            die "unexpected function call";
            say "#   Apply ", ($v[2]).perl;
            push $v, $value;
            return $v;
        }
        if $v[1] eq 'methcall' {
            say "#   Call ", ($v[2]).perl;
            $v = Call.new( invocant => $value, method => $v[2], arguments => $v[3], hyper => 0 );
            return $v;
        }
        if $v[1] eq 'funcall' {
            die "unexpected function call";
            say "#   Apply ", ($v[2]).perl;
            push $v, $value;
            return $v;
        }
        if $v[1] eq '( )' {
            say "#   Params ", ($v[2]).perl;
            if $value.isa('Apply') && !(defined($value.arguments)) {
                $value.arguments = $v[2];
                return $value;
            }
            if $value.isa('Call') && !(defined($value.arguments)) {
                $value.arguments = $v[2];
                return $value;
            }
            $v = Call.new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $v[2], hyper => 0 );
            return $v;
        }
        if $v[1] eq '[ ]' {
            say "#   Index ", ($v[2]).perl;
            $v = Index.new( obj => $value, index_exp => $v[2] );
            say "#     ", $v.perl;
            return $v;
        }
        if $v[1] eq '{ }' {
            say "#   Lookup ", ($v[2]).perl;
            $v = Lookup.new( obj => $value, index_exp => $v[2] );
            say "#     ", $v.perl;
            return $v;
        }
        if $v[1] eq 'block' {
            say "#   Lookup (was Block)", ($v[2]).perl;
            $v = Lookup.new( obj => $value, index_exp => ($v[2])[0] );
            return $v;
        }
        if $v[1] eq 'hash' {
            say "#   Lookup (was Hash or Pair)", ($v[2]).perl;
            $v = Lookup.new( obj => $value, index_exp => $v[2] );
            return $v;
        }
        if $v[1] eq '.( )' {
            $v = Call.new( invocant => $value, method => 'postcircumfix:<( )>', arguments => $v[2], hyper => 0 );
            return $v;
        }
        if $v[1] eq '.[ ]' {
            $v = Call.new( invocant => $value, method => 'postcircumfix:<[ ]>', arguments => $v[2], hyper => 0 );
            return $v;
        }
        if $v[1] eq '.{ }' {
            $v = Call.new( invocant => $value, method => 'postcircumfix:<{ }>', arguments => $v[2], hyper => 0 );
            return $v;
        }
        push $op, $value;
        return $op;
    }

    my $reduce_to_ast = sub ($op_stack, $num_stack) {
        my $last_op = $op_stack.shift;
        say "# reduce_to_ast ";
        say "#     last_op: ", $last_op.perl;
        say "#   num_stack: ", $num_stack;
        if $last_op[0] eq 'prefix' {
            push $num_stack,
                Apply.new(
                    namespace => '',
                    code      => 'prefix:<' ~ $last_op[1] ~ '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif $last_op[0] eq 'postfix' {
            push $num_stack,
                Apply.new(
                    namespace => '',
                    code      => 'postfix:<' ~ $last_op[1] ~ '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif $last_op[0] eq 'postfix_or_term' {
            $num_stack.push( reduce_postfix( $last_op, pop_term($num_stack) ) );
        }
        elsif MiniPerl6::Precedence::is_assoc_type('list', $last_op[1]) {
            my $arg;
            if $num_stack.elems < 2 {
                push $num_stack, 
                    Apply.new(
                        namespace => '',
                        code      => 'postfix:<' ~ $last_op[1] ~ '>',
                        arguments => [ pop_term($num_stack) ],
                      );
                return;
            }
            else {
                my $v2 = pop_term($num_stack);
                $arg = [ pop_term($num_stack), $v2 ];
            }
            if     (($arg[0]).isa('Apply'))
                && ($last_op[0] eq 'infix') 
                && (($arg[0]).code eq ('list:<' ~ $last_op[1] ~ '>')) 
            {
                push $num_stack,
                    Apply.new(
                        namespace => '',
                        code      => ($arg[0]).code,
                        arguments => [ @( ($arg[0]).arguments ), $arg[1] ],
                      );
                return;
            }
            push $num_stack, 
                Apply.new(
                    namespace => '',
                    code      => 'list:<' ~ $last_op[1] ~ '>',
                    arguments => $arg,
                  );
        }
        elsif MiniPerl6::Precedence::is_assoc_type('chain', $last_op[1]) {
            if $num_stack.elems < 2 {
                die "Missing value after operator";
            }
            my $v2 = pop_term($num_stack);
            my $arg = [ pop_term($num_stack), $v2 ];
            if ($arg[1]).isa('Hash')
                && MiniPerl6::Precedence::is_assoc_type('chain', ($arg[1]){op} ) 
            {
                push $num_stack,
                  {
                    op    => ['infix', $last_op],
                    val   => [ $arg[0] ],
                    chain => $arg[1]
                  };
                return;
            }
            push $num_stack, { op => ['infix', $last_op], val => $arg };
        }
        elsif $last_op[0] eq 'ternary' {
            if ( $num_stack.elems < 2 ) {
                die "Missing value after ternary operator";
            }
            my $v2 = pop_term($num_stack);
            push $num_stack,
                Apply.new(
                    namespace => '',
                    code      => 'ternary:<' ~ $last_op[1] ~ '>',
                    arguments => [ pop_term($num_stack), $last_op[2], $v2 ],
                  );
        }
        else {
            if ( $num_stack.elems < 2 ) {
                die "Missing value after operator";
            }
            my $v2 = pop_term($num_stack);
            push $num_stack,
                Apply.new(
                    namespace => '',
                    code      => 'infix:<' ~ $last_op[1] ~ '>',
                    arguments => [ pop_term($num_stack), $v2 ],
                  );
        }
    };
    
    # TODO
    # token pair {
    #     |   \: <var_sigil> <ident>                  #  :$var
    #         {
    #             make [
    #                 Val::Buf.new( buf => ~$<ident> ),
    #                 Var.new( sigil => ~$$<var_sigil>, twigil => '', name => $$<ident> ) ]
    #         }
    # };

    token operator { 
        | '.(' <paren_parse>   ')'                      { make [ 'postfix_or_term',  '.( )',  $$<paren_parse>   ] }
        | '.[' <square_parse>  ']'                      { make [ 'postfix_or_term',  '.[ ]',  $$<square_parse>  ] }
        | '.{' <curly_parse>   '}'                      { make [ 'postfix_or_term',  '.{ }',  $$<curly_parse>   ] }
        | '('  <paren_parse>   ')'                      { make [ 'postfix_or_term',  '( )',   $$<paren_parse>   ] }
        | '['  <square_parse>  ']'                      { make [ 'postfix_or_term',  '[ ]',   $$<square_parse>  ] }

        | '{'  <.MiniPerl6::Grammar.ws>?
            [ [  ### <before <.MiniPerl6::Grammar.pair> <.MiniPerl6::Grammar.ws>? ','>
              | <before 'pair' <!before <.MiniPerl6::Grammar.word> > >
              | <before '}' >
              ]
              <curly_parse> <.MiniPerl6::Grammar.ws>? '}'
                    { make [ 'postfix_or_term', 'hash',  $$<curly_parse> ] }
            | <MiniPerl6::Grammar.exp_stmts> <.MiniPerl6::Grammar.ws>? '}'
                    { make [ 'postfix_or_term', 'block', $$<MiniPerl6::Grammar.exp_stmts> ] }
            ]

        | '??' <ternary_parse> '!!'                     { make [ 'op',          '?? !!', $$<ternary_parse>  ] }
        ### | <MiniPerl6::Grammar.pair>                     { make [ 'term',        $$<MiniPerl6::Grammar.pair> ] }
        | '**'                                          { make [ 'op',          '**'  ] }
        | '++'                                          { make [ 'op',          '++'  ] }
        | '--'                                          { make [ 'op',          '--'  ] }
        | '||'                                          { make [ 'op',          '||'  ] }
        | '&&'                                          { make [ 'op',          '&&'  ] }
        | '//'                                          { make [ 'op',          '//'  ] }
        | '=>'                                          { make [ 'op',          '=>'  ] }
        | '=='                                          { make [ 'op',          '=='  ] }
        | '<='                                          { make [ 'op',          '<='  ] }
        | '!='                                          { make [ 'op',          '!='  ] }
        | ','                                           { make [ 'op',          ','   ] }
        | '+'                                           { make [ 'op',          '+'   ] }
        | '-'                                           { make [ 'op',          '-'   ] }
        | '|'                                           { make [ 'op',          '|'   ] }
        | '&'                                           { make [ 'op',          '&'   ] }
        | '?'                                           { make [ 'op',          '?'   ] }
        | '!'                                           { make [ 'op',          '!'   ] }
        | '/'                                           { make [ 'op',          '/'   ] }
        | '*'                                           { make [ 'op',          '*'   ] }
        | '<'                                           { make [ 'op',          '<'   ] }
        | '>'                                           { make [ 'op',          '>'   ] }
        | <MiniPerl6::Grammar.ident> <before <.MiniPerl6::Grammar.ws>? '=>' >   # autoquote
            { make [ 'term', Val::Buf.new( buf => ~$<MiniPerl6::Grammar.ident> ) ] }
        | 'and' <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'and' ] }
        | 'or'  <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'or'  ] }
        | 'not' <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'not' ] }
        | '.' <MiniPerl6::Grammar.ident> 
          [ <.MiniPerl6::Grammar.ws> <list_parse>   
            { make [ 'postfix_or_term', 'methcall',           ~$<MiniPerl6::Grammar.ident>, $$<list_parse> ] }
          | { make [ 'postfix_or_term', 'methcall_no_params', ~$<MiniPerl6::Grammar.ident>                 ] }
          ]
        | <MiniPerl6::Grammar.ident> 
          [ <.MiniPerl6::Grammar.ws> <list_parse>   
            { make [ 'postfix_or_term', 'funcall',            ~$<MiniPerl6::Grammar.ident>, $$<list_parse> ] }
          | { make [ 'postfix_or_term', 'funcall_no_params',  ~$<MiniPerl6::Grammar.ident>                 ] }
          ]
        | <MiniPerl6::Grammar.var_ident>                { make [ 'term', $$<MiniPerl6::Grammar.var_ident> ] }     
        | '$'                                           { make [ 'op',          '$'   ] }
        | '@'                                           { make [ 'op',          '@'   ] }
        | '%'                                           { make [ 'op',          '%'   ] }
        | <MiniPerl6::Grammar.val>                      { make [ 'term', $$<MiniPerl6::Grammar.val>       ] }
        | <.MiniPerl6::Grammar.ws>                      { make [ 'space',   ' '                           ] }
    }

    token list_lexer { 
        | 'and' <!before <.MiniPerl6::Grammar.word> >   { make [ 'end',     'and' ] }
        | 'or'  <!before <.MiniPerl6::Grammar.word> >   { make [ 'end',     'or'  ] }
        | ')'                                           { make [ 'end',     ')'   ] }
        | ';'                                           { make [ 'end',     ';'   ] }
        | <operator>                                    { make $$<operator> }
    }
    method list_parse ($str, $pos) {
        say "# list_parse: input ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $is_first_token = True;
        my $block_terminator = 0;
        my $get_token = sub {
            my $m = self.list_lexer($str, $last_pos);
            if !$m {
                return [ 'end', '*end*' ];
            }
            my $v = $$m;
            say "# list_lexer got " ~ $v.perl;

            # a sequence ( block - space - not_comma_or_colon ) terminates the list
            if ($block_terminator == 1) {
                if (($v[0]) eq 'space') {
                    # ignore
                }
                elsif ($block_terminator == 1) && (($v[0]) eq 'op') && (($v[1]) eq ',') { 
                    # TODO - test for colon
                    $block_terminator = 0;
                }
                else {
                    # end of list
                    $v[0] = 'end';
                }
            }
            elsif ($block_terminator == 0) && (($v[0]) eq 'postfix_or_term') && (($v[1]) eq 'block') {
                $block_terminator = 1;
            }

            if  $is_first_token 
                && ($v[0] eq 'op')
                && !(MiniPerl6::Precedence::is_fixity_type('prefix', $v[1]))
            {
                say "# finishing list - first token is: ", $v[1];
                $v[0] = 'end';
            }
            $is_first_token = False;   
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# list_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say "# list_lexer return: ", $res.perl;
        if $res.elems == 0 {
            return MiniPerl6::Match.new(bool => 0);
        }
        $res = pop_term($res);
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }


    token ternary_lexer { 
        | '!!'                          { make [ 'end',     '!!' ] }
        | <operator>                    { make $$<operator> }
    }
    method ternary_parse ($str, $pos) {
        say "# ternary_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.ternary_lexer($str, $last_pos);
            if !$m {
                die "Expected !! in ternary";
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# ternary_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        $res = pop_term($res);
        say "# ternary_parse return: ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }


    token curly_lexer { 
        | '}'                           { make [ 'end',     '}' ] }
        | <operator>                    { make $$<operator> }
    }
    method curly_parse ($str, $pos) {
        say "# curly_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.curly_lexer($str, $last_pos);
            if !$m {
                die "Expected closing '}'";
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# curly_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        $res = pop_term($res);
        say "# curly_parse return: ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }


    token square_lexer { 
        | ']'                           { make [ 'end',     ']' ] }
        | <operator>                    { make $$<operator> }
    }
    method square_parse ($str, $pos) {
        say "# square_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.square_lexer($str, $last_pos);
            if !$m {
                die "Expected closing ']'";
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# square_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        $res = pop_term($res);
        say "# square_parse return: ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }


    token paren_lexer { 
        | ')'                           { make [ 'end',     ')' ] }
        | <operator>                    { make $$<operator> }
    }
    method paren_parse ($str, $pos) {
        say "# paren_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.paren_lexer($str, $last_pos);
            if !$m {
                die "Expected closing parenthesis";
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# paren_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        $res = pop_term($res);
        say "# paren_parse return: ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res);
    }


    token exp_lexer { 
        | ';'                           { make [ 'end',     ';' ] }
        | '}'                           { make [ 'end',     '}' ] }
        | <operator>                    { make $$<operator> }
    }
    method exp_parse ($str, $pos) {
        say "# exp_parse input: ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.exp_lexer($str, $last_pos);
            if !$m {
                return [ 'end', '*end*' ];
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "# lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        if $res.elems == 0 {
            return MiniPerl6::Match.new(bool => 0);
        }
        $res = pop_term($res);
        say "# exp_parse result: ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res)
    } 

}

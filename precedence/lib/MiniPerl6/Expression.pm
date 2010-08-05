
class MiniPerl6::Expression {
    use MiniPerl6::Precedence;
    use MiniPerl6::Grammar;
    use MiniPerl6::Perl5::Emitter;
   
    sub pop_term ($num_stack) {
        my $v = $num_stack.pop;
        if $v.isa('Array') {
            say "** processing term ", $v.perl;
            if $v[1] eq 'methcall_no_params' {
                say "  Call ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq 'funcall_no_params' {
                say "  Apply ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq 'methcall' {
                say "  Call ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq 'funcall' {
                say "  Apply ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq '( )' {
                say "  Params ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq '[ ]' {
                say "  Index ", ($v[2]).perl;
                return $v;
            }
            if $v[1] eq '{ }' {
                say "  Lookup ", ($v[2]).perl;
                return $v;
            }
            return $v[1];
        }
        return $v;
    }

    sub reduce_postfix ($op, $value) {
        say "** reduce_postfix ", $op.perl;
        say "  ", $value.perl;
        push $op, $value;
        return $op;
    }

    my $reduce_to_ast = sub ($op_stack, $num_stack) {
        my $last_op = $op_stack.shift;
        if $last_op[0] eq 'prefix' {
            push $num_stack,
                Apply.new(
                    namespace => '',
                    code      => 'prefix:<' ~ $last_op[1] ~ '>',
                    arguments => [ pop_term($num_stack) ],
                  );
        }
        elsif $last_op[0] eq 'postfix' {
            $num_stack.push( reduce_postfix( $last_op, pop_term($num_stack) ) );
        }
        elsif MiniPerl6::Precedence::is_assoc_type('list', $last_op[1]) {
            my $arg;
            if $num_stack.elems < 2 {
                $arg = pop_term($num_stack);
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
                    arguments => [ pop_term($num_stack), $last_op, $v2 ],
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
    
    token operator { 
        | '('  <paren_parse>   ')'                      { make [ 'op_or_term',  '( )',   $$<paren_parse>   ] }
        | '['  <square_parse>  ']'                      { make [ 'op_or_term',  '[ ]',   $$<square_parse>  ] }
        | '{'  <curly_parse>   '}'                      { make [ 'op_or_term',  '{ }',   $$<curly_parse>   ] }
        | '??' <ternary_parse> '!!'                     { make [ 'op',          '?? !!', $$<ternary_parse> ] }
        | '++'                                          { make [ 'op',          '++'  ] }
        | '--'                                          { make [ 'op',          '--'  ] }
        | '||'                                          { make [ 'op',          '||'  ] }
        | '&&'                                          { make [ 'op',          '&&'  ] }
        | ','                                           { make [ 'op',          ','   ] }
        | '+'                                           { make [ 'op',          '+'   ] }
        | '-'                                           { make [ 'op',          '-'   ] }
        | '|'                                           { make [ 'op',          '|'   ] }
        | '&'                                           { make [ 'op',          '&'   ] }
        | '?'                                           { make [ 'op',          '?'   ] }
        | '!'                                           { make [ 'op',          '!'   ] }
        | 'and' <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'and' ] }
        | 'or'  <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'or'  ] }
        | 'not' <!before <.MiniPerl6::Grammar.word> >   { make [ 'op',          'not' ] }
        | '.' <MiniPerl6::Grammar.ident> 
          [ <.MiniPerl6::Grammar.ws> <list_parse>   
            { make [ 'op_or_term', 'methcall',           ~$<MiniPerl6::Grammar.ident>, $$<list_parse> ] }
          | { make [ 'op_or_term', 'methcall_no_params', ~$<MiniPerl6::Grammar.ident>                 ] }
          ]
        | <MiniPerl6::Grammar.ident> 
          [ <.MiniPerl6::Grammar.ws> <list_parse>   
            { make [ 'op_or_term', 'funcall',            ~$<MiniPerl6::Grammar.ident>, $$<list_parse> ] }
          | { make [ 'op_or_term', 'funcall_no_params',  ~$<MiniPerl6::Grammar.ident>                 ] }
          ]
        | <MiniPerl6::Grammar.var_ident>                { make [ 'term', $$<MiniPerl6::Grammar.var_ident> ] }     
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
        say "list_parse ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $is_first_token = True;
        my $get_token = sub {
            my $m = self.list_lexer($str, $last_pos);
            if !$m {
                return [ 'end', '*end*' ];
            }
            my $v = $$m;
            if  $is_first_token 
                && ($v[0] eq 'op')
                && !(MiniPerl6::Precedence::is_fixity_type('prefix', $v[1]))
            {
                say "finishing list - first token is: ", $v[1];
                $v[0] = 'end';
            }
            $is_first_token = False;   
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "list_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say 'list_lexer return: ', $res.perl;
        if $res.elems == 0 {
            return MiniPerl6::Match.new(bool => 0);
        }
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0]);
    }


    token ternary_lexer { 
        | '!!'                          { make [ 'end',     '!!' ] }
        | <operator>                    { make $$<operator> }
    }
    method ternary_parse ($str, $pos) {
        say "ternary_parse ",$str," at ",$pos;
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
            say "ternary_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0]);
    }


    token curly_lexer { 
        | '}'                           { make [ 'end',     '}' ] }
        | <operator>                    { make $$<operator> }
    }
    method curly_parse ($str, $pos) {
        say "curly_parse ",$str," at ",$pos;
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
            say "curly_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0]);
    }


    token square_lexer { 
        | ']'                           { make [ 'end',     ']' ] }
        | <operator>                    { make $$<operator> }
    }
    method square_parse ($str, $pos) {
        say "square_parse ",$str," at ",$pos;
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
            say "square_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0]);
    }


    token paren_lexer { 
        | ')'                           { make [ 'end',     ')' ] }
        | <operator>                    { make $$<operator> }
    }
    method paren_parse ($str, $pos) {
        say "paren_parse ",$str," at ",$pos;
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
            say "paren_lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0]);
    }


    token lexer { 
        | ';'                           { make [ 'end',     ';' ] }
        | '}'                           { make [ 'end',     '}' ] }
        | <operator>                    { make $$<operator> }
    }
    method exp_parse ($str, $pos) {
        say "exp_parse: input ",$str," at ",$pos;
        my $expr;
        my $last_pos = $pos;
        my $get_token = sub {
            my $m = self.lexer($str, $last_pos);
            if !$m {
                return [ 'end', '*end*' ];
            }
            my $v = $$m;
            if $v[0] ne 'end' {
                $last_pos = $m.to;
            }
            say "lexer " ~ $v.perl;
            return $v;
        };
        my $res = MiniPerl6::Precedence::precedence_parse($get_token, $reduce_to_ast);
        say "exp_parse: result ", $res.perl;
        return MiniPerl6::Match.new( 
            'str' => $str, 'from' => $pos, 'to' => $last_pos, 'bool' => 1, capture => $res[0])
    } 

}


class MiniPerl6::Precedence {

    my $Operator;
    my $Precedence;    # integer 0..100
    my $Assoc;         # right, left, list
    my $Allow_space_before;
    
    sub is_assoc_type ($assoc_type, $op_name) {
        return ($Assoc{$assoc_type}){$op_name} 
    }

    sub add_op ( $fixity, $name, $precedence, $param ) {
        if !(defined($param)) {
            $param = {}
        }
        my $second_op = $param{'second_op'};
        my $assoc = $param{'assoc'} || 'left';
        ($Operator{$fixity}){$name} = $second_op || 1;
        $Precedence{$name}          = $precedence;
        ($Assoc{$assoc}){$name}     = 1;
        ($Allow_space_before{$fixity}){$name} = $param{'no_space_before'} ?? False !! True;
    }
    

    # s02:
    # - no space allowed before postfix ops
    # - if there is both an infix and a postfix with the same name, then the infix requires space before
    # - 'reduce' vs. 'array' [...]
    # - @a[] inside string interpolation
    # - longest token
    # - parentheses vs. Parcel (x) (x,)
    # - pair vs. block, hash vs. closure
    # - adverbs  1 == 100 :fuzz(3)
    # - function call without parenthesis
    # - '|' in prefix position

    my $prec = 100;
    add_op( 'infix',    '.',   $prec, { no_space_before => True } );
    add_op( 'prefix',   '.',   $prec );
    add_op( 'postcircumfix', '(', $prec, { second_op => ')', no_space_before => True } );
    add_op( 'postcircumfix', '{', $prec, { second_op => '}', no_space_before => True } );
    add_op( 'postcircumfix', '[', $prec, { second_op => ']', no_space_before => True } );
    $prec = $prec - 1;
    add_op( 'prefix',   '++',  $prec );
    add_op( 'prefix',   '--',  $prec );
    add_op( 'postfix',  '++',  $prec, { no_space_before => True } );
    add_op( 'postfix',  '--',  $prec, { no_space_before => True } );
    $prec = $prec - 1;
    add_op( 'infix',    '**',  $prec, { assoc => 'right' } );
    $prec = $prec - 1;
    add_op( 'prefix',   '+',   $prec );
    add_op( 'prefix',   '-',   $prec );
    add_op( 'prefix',   '$',   $prec );
    add_op( 'prefix',   '@',   $prec );
    add_op( 'prefix',   '%',   $prec );
    add_op( 'prefix',   '!',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '*',   $prec );
    add_op( 'infix',    '/',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '+',   $prec );
    add_op( 'infix',    '-',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '&',   $prec, { assoc => 'list' } );
    add_op( 'prefix',   '&',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '|',   $prec, { assoc => 'list' } );
    add_op( 'prefix',   '|',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    'ne',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'eq',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '==',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<',   $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>',   $prec, { assoc => 'chain' } );
    $prec = $prec - 1;
    add_op( 'infix',    '&&',  $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '||',  $prec );
    $prec = $prec - 1;
    add_op( 'ternary',  '??',  $prec, { second_op => '!!' } );
    $prec = $prec - 1;
    add_op( 'infix',    '=',   $prec, { assoc => 'right' } );
    add_op( 'infix',    ':=',  $prec, { assoc => 'right' } );
    $prec = $prec - 1;
    add_op( 'prefix',   'not', $prec );
    $prec = $prec - 1;
    add_op( 'list',     ',',   $prec, { assoc => 'list' } );
    $prec = $prec - 1;
    add_op( 'list',     ';',   $prec, { assoc => 'list' } );
    $prec = $prec - 1;
    add_op( 'infix',    'and', $prec );
    $prec = $prec - 1;
    add_op( 'infix',    'or',  $prec );
    $prec = $prec - 1;
    add_op( 'circumfix', '(',  $prec, { second_op => ')' } );
    add_op( 'circumfix', '[',  $prec, { second_op => ']' } );
    add_op( 'circumfix', '{',  $prec, { second_op => '}' } );
    $prec = $prec - 1;
    add_op( 'infix',    '*start*', $prec );
    
    sub precedence_parse ($get_token, $reduce) {
        my $op_stack  = [];   # [category, name]
        my $num_stack = [];
        my $last = ['op', '*start*'];
        my $last_has_space = False;
        my $token = $get_token.();
        if ($token[0]) eq 'space' {
            $token = $get_token.()
        }
        while (defined($token)) && ($token[0] ne 'end') {
            if ($Operator{'prefix'}){$token[1]}
                && ( ($last[1] eq '*start*') || (($last[0]) ne 'term') )
            {
                $op_stack.unshift( ['prefix', $token[1]] );
            }
            elsif ($Operator{'postfix'}){$token[1]} && (($last[0]) eq 'term') 
                && (  ($Allow_space_before{'postfix'}){$token[1]} 
                   || !$last_has_space 
                   )
            {
                my $pr = $Precedence{$token[1]};
                while $op_stack.elems
                    && ($pr <= $Precedence{ ($op_stack[0])[1] })
                {
                    $reduce.($op_stack, $num_stack);
                }
                $op_stack.unshift( ['postfix', $token[1]] );
            }
            elsif ($Operator{'postcircumfix'}){$token[1]} && (($last[0]) eq 'term') 
                && (  ($Allow_space_before{'postcircumfix'}){$token[1]} 
                   || !$last_has_space 
                   )
            {
                # last term was a value
                my $pr = $Precedence{$token[1]};
                while $op_stack.elems
                    && ( $pr <= $Precedence{ ($op_stack[0])[1] } )
                {
                    $reduce.($op_stack, $num_stack);
                }
                my $res = precedence_parse($get_token);
                my $pre_term = pop($num_stack);
                if ($token[1] eq '(') 
                    && $pre_term.isa('Hash') 
                    && ((($pre_term{'op'})[0] eq 'prefix') || (($pre_term{'op'})[0] eq 'infix'))
                    && (($pre_term{'op'})[1] eq '.')
                {
                    # term.meth(...)
                    push $num_stack,
                      {
                        op  => $pre_term{'op'},
                        val => [ @($pre_term{'val'}), $res[0] ]
                      };
                }
                else {
                    push $num_stack,
                      {
                        op  => ['postcircumfix', $token[1] ~ ' ' ~ ($Operator{'postcircumfix'}){$token[1]}],
                        val => [ $pre_term, $res[0] ]
                      };
                }
                $token = ['term', '0'];
            }
            elsif ($Operator{'circumfix'}){$token[1]} {
                if ($last[0]) eq 'term' {
                    die "Value tokens must be separated by an operator";
                }
                my $res = precedence_parse($get_token);
                $num_stack.push(
                  {
                    op  => ['circumfix', $token[1] ~ ' ' ~ ($Operator{'circumfix'}){$token[1]}],
                    val => $res[0]
                  } );
                $token = ['term', '0'];
            }
            elsif ($token[0]) eq 'term' {
                if ($last[0]) eq 'term' {
                    say "term 0: ", $last.perl;
                    say "term 1: ", $token.perl;
                    die "Value tokens must be separated by an operator";
                }
                $num_stack.push( $token[1] );
            }
            elsif  ($Operator{'infix'}){$token[1]}    
                || ($Operator{'list'}){$token[1]} 
                || ($Operator{'ternary'}){$token[1]} 
            {   
                my $pr = $Precedence{$token[1]};
                if ($Assoc{'right'}){$token[1]} {
                    while $op_stack.elems
                        && ( $pr < $Precedence{ ($op_stack[0])[1] } )
                    {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                else {
                    while $op_stack.elems
                        && ( $pr <= $Precedence{ ($op_stack[0])[1] } )
                    {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                if ($Operator{'ternary'}){$token[1]} {
                    $op_stack.unshift( [ 'ternary', 
                                         $token[1],
                                         ($Operator{'ternary'}){$token[1]}, 
                                         $token[2] ] );
                }
                else {
                    $op_stack.unshift( ['infix', $token[1]] );
                }
            }
            else {
                die "Unknown token: '", $token[1], "'";
            }
            $last = $token;
            $token = $get_token.();
            if $token[0] eq 'space' {
                $token = $get_token.()
                $last_has_space = True;
            }
            else {
                $last_has_space = False;
            }
        }
        if defined($token) && ($token[0] ne 'end') {
            die "Unexpected end token: ",$token.perl;
        }
        while $op_stack.elems {
            $reduce.($op_stack, $num_stack);
        }
        return $num_stack[0];
    }
}


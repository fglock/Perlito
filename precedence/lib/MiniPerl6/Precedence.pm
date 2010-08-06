
class MiniPerl6::Precedence {

    my $Operator;
    my $Precedence;    # integer 0..100
    my $Assoc;         # right, left, list
    my $Allow_space_before;
    
    sub is_assoc_type ($assoc_type, $op_name) {
        return ($Assoc{$assoc_type}){$op_name} 
    }

    sub is_fixity_type ($fixity_type, $op_name) {
        return ($Operator{$fixity_type}){$op_name}
    }

    sub add_op ( $fixity, $name, $precedence, $param ) {
        if !(defined($param)) {
            $param = {}
        }
        my $assoc = $param{'assoc'} || 'left';
        ($Operator{$fixity}){$name} = 1;
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
    # - function call without parentheses
    # - '|' in prefix position
    # - statement-ending blocks (S04)

    my $prec = 100;
    add_op( 'postfix', '.( )',               $prec, { no_space_before => True } );
    add_op( 'postfix', '.[ ]',               $prec, { no_space_before => True } );
    add_op( 'postfix', '.{ }',               $prec, { no_space_before => True } );
    add_op( 'postfix', '( )',                $prec, { no_space_before => True } );
    add_op( 'postfix', '[ ]',                $prec, { no_space_before => True } );
    add_op( 'postfix', 'funcall',            $prec, { no_space_before => True } );
    add_op( 'postfix', 'funcall_no_params',  $prec, { no_space_before => True } );
    add_op( 'postfix', 'methcall',           $prec, { no_space_before => True } );
    add_op( 'postfix', 'methcall_no_params', $prec, { no_space_before => True } );
    add_op( 'postfix', 'block',              $prec, { no_space_before => True } );
    add_op( 'postfix', 'hash',               $prec, { no_space_before => True } );
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
    add_op( 'infix',    '//',  $prec );
    $prec = $prec - 1;
    add_op( 'ternary',  '?? !!',  $prec );
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
    add_op( 'infix',    '*start*', $prec );
    
    sub is_term ($token) {
        ($token[0] eq 'term') || ($token[0] eq 'postfix_or_term')
    }

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
            if ($Operator{'prefix'}){$token[1]} && ( ($last[1] eq '*start*') || !(is_term($last)) ) {
                $token[0] = 'prefix';
                $op_stack.unshift($token);
            }
            elsif ($Operator{'postfix'}){$token[1]} && is_term($last) 
                && (  ($Allow_space_before{'postfix'}){$token[1]} 
                   || !($last_has_space) 
                   )
            {
                my $pr = $Precedence{$token[1]};
                while $op_stack.elems && ($pr <= $Precedence{ ($op_stack[0])[1] }) {
                    $reduce.($op_stack, $num_stack);
                }
                if ($token[0]) ne 'postfix_or_term' {
                    $token[0] = 'postfix';
                }
                $op_stack.unshift($token);
            }
            elsif ($token[1] eq 'block') && is_term($last) && $last_has_space {
                # a block in this position terminates the current expression
                while $op_stack.elems {
                    $reduce.($op_stack, $num_stack);
                }
                return $num_stack;
            }
            elsif is_term($token) {
                say "# ** two terms in a row ";
                say "#      last:  ", $last.perl;
                say "#      token: ", $token.perl;
                say "#      space: ", $last_has_space;

                if is_term($last) {
                    die "Value tokens must be separated by an operator";
                }
                $token[0] = 'term';
                $num_stack.push($token);
            }
            elsif $Precedence{$token[1]} {   
                my $pr = $Precedence{$token[1]};
                if ($Assoc{'right'}){$token[1]} {
                    while $op_stack.elems && ( $pr < $Precedence{ ($op_stack[0])[1] } ) {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                else {
                    while $op_stack.elems && ( $pr <= $Precedence{ ($op_stack[0])[1] } ) {
                        $reduce.($op_stack, $num_stack);
                    }
                }
                if ($Operator{'ternary'}){$token[1]} {
                    $token[0] = 'ternary';
                }
                else {
                    $token[0] = 'infix';
                }
                $op_stack.unshift($token);
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
        return $num_stack;
    }
}


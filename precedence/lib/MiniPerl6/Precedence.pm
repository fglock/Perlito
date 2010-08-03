
class MiniPerl6::Precedence {

    my $Operator;
    my $Precedence;    # integer 0..100
    my $Assoc;         # right, left, list
    my $Reserved;
    my $Allow_space_before;
    
    sub add_op ( $fixity, $name, $precedence, $param ) {
        if !(defined($param)) {
            $param = {}
        }
        my $second_op = $param{'second_op'};
        my $assoc = $param{'assoc'} || 'left';
        # my $full_name = $fixity ~ ":" ~ $name ~ ( $second_op ?? " " ~ $second_op !! "" );
        ($Operator{$fixity}){$name} = $second_op || 1;
        # $Precedence{$full_name}   = $precedence;
        $Precedence{$name}        = $precedence;
        ($Assoc{$assoc}){$name}     = 1;
        ($Allow_space_before{$fixity}){$name} = $param{'no_space_before'} ?? False !! True;
        $Reserved{$name}          = 1;
        # $Reserved{$full_name}     = 1;
        if $second_op {
            $Reserved{$second_op}  = 1
        }
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
    
    # say $Operator.perl;
    # say $Precedence.perl;    # integer
    # say $Assoc.perl;         # right, left, list
    # say $Reserved.perl;
    say $Allow_space_before.perl;

    sub precedence_parse ($get_token, $end_token) {
        my $op_stack  = [];   # [category, name]
        my $num_stack = [];
        my $last = ['op', '*start*'];
        my $last_has_space = False;
    
        my $exec = sub {
            my $last_op = $op_stack.shift;
            say("EXEC OP ", $last_op.perl);
            if $last_op[0] eq 'prefix' {
                push $num_stack,
                  { op => $last_op, val => [ pop($num_stack) ] };
            }
            elsif $last_op[0] eq 'postfix' {
                push $num_stack,
                  { op => $last_op, val => [ pop($num_stack) ] };
            }
            elsif ($Assoc{'list'}){ $last_op[1] } {
                say "num_stack is ", $num_stack.elems;
                my $arg;
                if $num_stack.elems < 2 {
                    # die "Missing value after operator";
                    $arg = [ pop($num_stack) ];
                    push $num_stack, { op => [ 'postfix', $last_op[1] ], val => $arg };
                    return;
                }
                else {
                    $arg = [ pop($num_stack), pop($num_stack) ];
                }
                say "Assoc list ", $arg.perl;
                if     (($arg[1]).isa('Hash'))
                    && ($last_op[0] eq 'infix') 
                    && (((($arg[1]){'op'})[0]) eq 'list') 
                    && ($last_op[1] eq ((($arg[1]){'op'})[1])) 
                {
                    say "LISTOP: '$last_op' '$arg[1]{'op'}'";
                    push $num_stack,
                      {
                        op  => $last_op,
                        val => [ $arg[0], @( ($arg[1]){'val'} ) ]
                      };
                    return;
                }
                push $num_stack, { op => ['list', $last_op[1]], val => $arg };
            }
            elsif ($Assoc{'chain'}){ $last_op } {
                if $num_stack < 2 {
                    die "Missing value after operator";
                }
                my $arg = [ pop($num_stack), pop($num_stack) ];
                say "ASSOC CHAIN $arg";
                if ($arg[1]).isa('Hash')
                    && ($Assoc{'chain'}){ ($arg[1]){op} } 
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
                say "EXEC TERNARY";
                say "  $num_stack   $last_op";
                if ( $num_stack < 3 ) {
                    die "Missing value after ternary operator";
                }
                push $num_stack,
                  {
                    op  => $last_op,
                    val => [ pop($num_stack), pop($num_stack), pop($num_stack) ]
                  };
            }
            else {
                say "EXEC INFIX";
                if ( $num_stack.elems < 2 ) {
                    die "Missing value after operator";
                }
                push $num_stack,
                  {
                    op  => $last_op,
                    val => [ pop($num_stack), pop($num_stack) ]
                  };
            }
        };
    
        say "op_stack init ", $op_stack.perl;
        my $token = $get_token.();
        if ($token[0]) eq 'space' {
            $token = $get_token.()
        }
        while (defined($token)) && ($token[1] ne $end_token) {
            say "token: '",$last.perl,"' '",$token.perl,"'";
            say "         op: ", $op_stack.perl;
            if ($Operator{'prefix'}){$token[1]}
                && ( ($last[1] eq '*start*') || (($last[0]) ne 'term') )
            {
                say "PREFIX";
                $op_stack.unshift( ['prefix', $token[1]] );
            }
            elsif ($Operator{'postfix'}){$token[1]} && (($last[0]) eq 'term') 
                && (  ($Allow_space_before{'postfix'}){$token[1]} 
                   || !$last_has_space 
                   )
            {
                say "POSTFIX";
                my $pr = $Precedence{$token[1]};
                while $op_stack 
                    && ($pr <= $Precedence{ ($op_stack[0])[1] })
                {
                    $exec.();
                }
                $op_stack.unshift( ['postfix', $token[1]] );
            }
            elsif ($Operator{'ternary'}){$token[1]} {
                say "TERNARY #1 ";
                my $pr = $Precedence{$token[1]};
                while $op_stack 
                    && ($pr <= $Precedence{ ($op_stack[0])[1] } )
                {
                    $exec.();
                }
                my $res = (precedence_parse($get_token, ($Operator{'ternary'}){$token[1]} ))[0];
                say "TERNARY #2 ", $res.perl;
                push $num_stack, $res;
                $op_stack.unshift( ['ternary', $token[1] ~ ' ' ~ ($Operator{'ternary'}){$token[1]} ] );
            }
            elsif ($Operator{'postcircumfix'}){$token[1]} && (($last[0]) eq 'term') 
                && (  ($Allow_space_before{'postcircumfix'}){$token[1]} 
                   || !$last_has_space 
                   )
            {
                # last term was a value
                say "postcircumfix start: '",$token[1],"' space:", $last_has_space, " allow_space:", (($Allow_space_before{'postcircumfix'}){$token[1]});
                my $pr = $Precedence{$token[1]};
                while $op_stack 
                    && ( $pr <= $Precedence{ ($op_stack[0])[1] } )
                {
                    $exec.();
                }
                my $res = precedence_parse($get_token, ($Operator{'postcircumfix'}){$token[1]} );
                my $pre_term = pop($num_stack);
                say $res.perl;
                say "pre_term: ", $pre_term.perl, " token: ", $token[1];
                if ($token[1] eq '(') 
                    && $pre_term.isa('Hash') 
                    && ((($pre_term{'op'})[0] eq 'prefix') || (($pre_term{'op'})[0] eq 'infix'))
                    && (($pre_term{'op'})[1] eq '.')
                {
                    # term.meth(...)
                    say "term.meth(...)";
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
                say "circumfix start: $token[1] ";
                my $res = precedence_parse($get_token, ($Operator{'circumfix'}){$token[1]} );
                say $res.perl;
                $num_stack.push(
                  {
                    op  => ['circumfix', $token[1] ~ ' ' ~ ($Operator{'circumfix'}){$token[1]}],
                    val => $res[0]
                  } );
                $token = ['term', '0'];
            }
            elsif ($token[0]) eq 'term' {
                if ($last[0]) eq 'term' {
                    die "Value tokens must be separated by an operator";
                }
                say "  push num ",$token[1];
                $num_stack.push( $token[1] );
            }
            elsif ($Operator{'infix'}){$token[1]}    
                || ($Operator{'list'}){$token[1]} 
            {   
                my $pr = $Precedence{$token[1]};
                say "  is token";
                say "      : ", $op_stack.perl;
                say "infix start: '",$token[1],"' space:", $last_has_space, " allow_space:", (($Allow_space_before{'infix'}){$token[1]});

                if ($Assoc{'right'}){$token[1]} {
                    while $op_stack 
                        && ( $pr < $Precedence{ ($op_stack[0])[1] } )
                    {
                        $exec.();
                    }
                }
                else {
                    while $op_stack 
                        && ( $pr <= $Precedence{ ($op_stack[0])[1] } )
                    {
                        $exec.();
                    }
                }
                # say "  before op: ", $op_stack.perl;
                # say "    save op: ", ['infix', $token[1]].perl;
                $op_stack.unshift( ['infix', $token[1]] );
                # say "         op: ", $op_stack.perl;
            }
            else {
                die "Unknown token: '", $token[1], "'";
            }
            $last = $token;
            $token = $get_token.();
            if ($token[0] eq 'space') {
                $token = $get_token.()
                $last_has_space = True;
            }
            else {
                $last_has_space = False;
            }
        }
        say "FINISH AT '",$token.perl,"'";
        say "op_stack ", $op_stack.perl;
        if $end_token && ($token[1] ne $end_token) {
            die "Expected $end_token";
        }
        while $op_stack {
            $exec.();
        }
        return $num_stack;
    }
}


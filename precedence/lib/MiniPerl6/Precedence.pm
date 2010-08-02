
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
    add_op( 'infix',    'ne',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    'eq',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>=',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '==',  $prec, { assoc => 'chain' } );
    add_op( 'infix',    '<',   $prec, { assoc => 'chain' } );
    add_op( 'infix',    '>',   $prec, { assoc => 'chain' } );
    $prec = $prec - 1;
    add_op( 'infix',    '&&',  $prec );
    add_op( 'infix',    '&',   $prec );
    $prec = $prec - 1;
    add_op( 'infix',    '||',  $prec );
    add_op( 'infix',    '|',   $prec );
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

    sub is_term ($s) {
        !($Reserved{$s})
    }
    
    sub is_space ($s) {
        ($s eq '') || substr($s, 0, 1) eq ' '
    }

    sub precedence_parse ($get_token, $end_token) {
        my $op_stack  = [];   # [category, name]
        my $num_stack = [];
        my $last = "*start*";
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
                if $num_stack < 2 {
                    die "Missing value after operator";
                }
                my $arg = [ pop($num_stack), pop($num_stack) ];
                if     (($arg[1]).isa('Hash'))
                    && (($Assoc{'list'}){ ($arg[1]){'op'} })
                    && ($last_op eq (($arg[1]){'op'})) 
                {
                    say "LISTOP: '$last_op' '$arg[1]{'op'}'";
                    push $num_stack,
                      {
                        op  => $last_op,
                        val => [ $arg[0], @( ($arg[1]){val} ) ]
                      };
                    return;
                }
                push $num_stack, { op => $last_op, val => $arg };
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
            # elsif ($Operator{'ternary'}){ $last_op }  {
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
                if ( $num_stack < 2 ) {
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
        if is_space($token) {
            $token = $get_token.()
        }
        while ($token ne '') && ($token ne $end_token) {
            say "token: '",$last,"' '",$token,"'";
            say "         op: ", $op_stack.perl;
            if ($Operator{'prefix'}){$token}
                && ( ($last eq '*start*') || !(is_term($last)) )
            {
                say "PREFIX";
                $op_stack.unshift( ['prefix', $token] );
            }
            elsif ($Operator{'postfix'}){$token} && is_term($last) 
                && (  ($Allow_space_before{'postfix'}){$token} 
                   || !$last_has_space 
                   )
            {
                say "POSTFIX";
                my $pr = $Precedence{$token};
                while $op_stack 
                    && ($pr <= $Precedence{ ($op_stack[0])[1] })
                {
                    $exec.();
                }
                $op_stack.unshift( ['postfix', $token] );
            }
            elsif ($Operator{'ternary'}){$token} {
                say "TERNARY #1 ";
                my $pr = $Precedence{$token};
                while $op_stack 
                    && ($pr <= $Precedence{ ($op_stack[0])[1] } )
                {
                    $exec.();
                }
                my $res = (precedence_parse($get_token, ($Operator{'ternary'}){$token} ))[0];
                say "TERNARY #2 ", $res.perl;
                push $num_stack, $res;
                $op_stack.unshift( ['ternary', $token ~ ' ' ~ ($Operator{'ternary'}){$token} ] );
            }
            elsif ($Operator{'postcircumfix'}){$token} && is_term($last) 
                && (  ($Allow_space_before{'postcircumfix'}){$token} 
                   || !$last_has_space 
                   )
            {
                # last term was a value
                say "postcircumfix start: '",$token,"' space:", $last_has_space, " allow_space:", (($Allow_space_before{'postcircumfix'}){$token});
                my $pr = $Precedence{$token};
                while $op_stack 
                    && ( $pr <= $Precedence{ ($op_stack[0])[1] } )
                {
                    $exec.();
                }
                my $res = precedence_parse($get_token, ($Operator{'postcircumfix'}){$token} );
                my $pre_term = pop($num_stack);
                say $res.perl;
                say "pre_term: ", $pre_term.perl, " token: ", $token;
                if ($token eq '(') 
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
                        op  => ['postcircumfix', $token ~ ' ' ~ ($Operator{'postcircumfix'}){$token}],
                        val => [ $pre_term, $res[0] ]
                      };
                }
                $token = '0';
            }
            elsif ($Operator{'circumfix'}){$token} {
                if ( is_term($last) ) {
                    die "Value tokens must be separated by an operator";
                }
                say "circumfix start: $token ";
                my $res = precedence_parse($get_token, ($Operator{'circumfix'}){$token} );
                say $res.perl;
                $num_stack.push(
                  {
                    op  => ['circumfix', $token ~ ' ' ~ ($Operator{'circumfix'}){$token}],
                    val => $res[0]
                  } );
                $token = '0';
            }
            elsif ( is_term($token) ) {
                if ( is_term($last) ) {
                    die "Value tokens must be separated by an operator";
                }
                say "  push num ",$token;
                $num_stack.push( $token );
            }
            elsif ($Operator{'infix'}){$token}    
                || ($Operator{'list'}){$token} 
            {   
                my $pr = $Precedence{$token};
                say "  is token";
                say "      : ", $op_stack.perl;

                say "infix start: '",$token,"' space:", $last_has_space, " allow_space:", (($Allow_space_before{'infix'}){$token});
                if $last_has_space
                    && !( ($Allow_space_before{'postcircumfix'}){$token} )
                {
                    die "Value tokens must be separated by an operator";
                }

                if ($Assoc{'right'}){$token} {
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
                # say "    save op: ", ['infix', $token].perl;
                $op_stack.unshift( ['infix', $token] );
                # say "         op: ", $op_stack.perl;
            }
            else {
                die "Unknown token: '", $token, "'";
            }
            $last = $token;
            $token = $get_token.();
            if is_space($token) {
                $token = $get_token.()
                $last_has_space = True;
            }
            else {
                $last_has_space = False;
            }
        }
        say "FINISH AT '",$token,"'";
        say "op_stack ", $op_stack.perl;
        if $end_token && ($token ne $end_token) {
            die "Expected $end_token";
        }
        while $op_stack {
            $exec.();
        }
        return $num_stack;
    }
}


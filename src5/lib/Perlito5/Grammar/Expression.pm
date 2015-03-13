
package Perlito5::Grammar::Expression;
use Perlito5::Grammar::Precedence;
use Perlito5::Grammar::Bareword;
use Perlito5::Grammar::Attribute;
use Perlito5::Grammar::Statement;

Perlito5::Grammar::Precedence::add_term( 'my'    => sub { Perlito5::Grammar::Expression->term_declarator( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( 'our'   => sub { Perlito5::Grammar::Expression->term_declarator( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( 'eval'  => sub { Perlito5::Grammar::Expression->term_eval( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( 'state' => sub { Perlito5::Grammar::Expression->term_declarator( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( 'local' => sub { Perlito5::Grammar::Expression->term_local( $_[0], $_[1] ) } );
Perlito5::Grammar::Precedence::add_term( 'return' => sub { Perlito5::Grammar::Expression->term_return( $_[0], $_[1] ) } );


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
    return Perlito5::AST::Apply->new( code => 'circumfix:<{ }>', namespace => '', arguments => expand_list($stmt));
}

sub pop_term {
    my $num_stack = shift;
    my $v = pop @$num_stack;
    if (ref($v) eq 'ARRAY') {
        # say "# ** processing term ", $v->perl;
        return $v->[1] if ref($v->[1]);     # optimization - avoid strigifying objects
        if ($v->[1] eq 'methcall_no_params') {
            # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
            $v = Perlito5::AST::Call->new( invocant => undef, method => $v->[2], arguments => [] );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'funcall_no_params') {
            # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
            $v = Perlito5::AST::Apply->new( code => $v->[3], namespace => $v->[2], arguments => [], bareword => 1 );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'methcall') {
            # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
            my $param_list = expand_list( ($v->[3]) );
            $v = Perlito5::AST::Call->new( invocant => undef, method => $v->[2], arguments => $param_list );
            # say "#     ", $v->perl;
            return $v;
        }
        if ($v->[1] eq 'funcall') {
            # say "#   Perlito5::AST::Apply ", ($v->[2])->perl;
            my $param_list = expand_list( ($v->[4]) );
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
        die "Bareword found where operator expected";
    }
    if ($v->[1] eq 'methcall') {
        # say "#   Perlito5::AST::Call ", ($v->[2])->perl;
        my $param_list = expand_list($v->[3]);
        $v = Perlito5::AST::Call->new( invocant => $value, method => $v->[2], arguments => $param_list );
        return $v;
    }
    if ($v->[1] eq 'funcall') {
        die "unexpected function call";
    }
    if ($v->[1] eq '( )') {
        # say "#   Params ", ($v->[2])->perl;
        my $param_list = expand_list($v->[2]);
        if ( ref($value) eq 'Perlito5::AST::Apply' && !(defined($value->arguments))) {
            $value->{arguments} = $param_list;
            return $value;
        }
        if ( ref($value) eq 'Perlito5::AST::Call' && !(defined($value->arguments))) {
            $value->{arguments} = $param_list;
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
    elsif (Perlito5::Grammar::Precedence::is_assoc_type('list', $last_op->[1])) {
        my $arg;
        if (scalar(@$num_stack) < 2) {
            my $v2 = pop_term($num_stack);
            if ( ref($v2) eq 'Perlito5::AST::Apply' && $v2->code eq ('list:<' . $last_op->[1] . '>')) {
                push @$num_stack,
                    Perlito5::AST::Apply->new(
                        namespace => $v2->namespace,
                        code      => $v2->code,
                        arguments => [ @{ $v2->arguments } ],
                      );
            }
            else {
                push @$num_stack,
                    Perlito5::AST::Apply->new(
                        namespace => '',
                        code      => 'list:<' . $last_op->[1] . '>',
                        arguments => [ $v2 ],
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
    elsif (Perlito5::Grammar::Precedence::is_assoc_type('chain', $last_op->[1])) {
        if (scalar(@$num_stack) < 2) {
            die("Missing value after operator " . $last_op->[1]);
        }
        my $v2 = pop_term($num_stack);
        my $arg = [ pop_term($num_stack), $v2 ];
        # say "# assoc chain: ", $arg->perl;
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
    '->' <.Perlito5::Grammar::Space.opt_ws>
        [
        | '(' <paren_parse>   ')'                   { $MATCH->{capture} = [ 'postfix_or_term',  '.( )',  Perlito5::Match::flat($MATCH->{paren_parse})   ] }
        | '[' <square_parse>  ']'                   { $MATCH->{capture} = [ 'postfix_or_term',  '.[ ]',  Perlito5::Match::flat($MATCH->{square_parse})  ] }
        | '{' <curly_parse>
            [ \} | { die 'Missing right curly or square bracket' } ]
            { $MATCH->{capture} = [ 'postfix_or_term',  '.{ }',  Perlito5::Match::flat($MATCH->{curly_parse})   ] }

        | '$' <Perlito5::Grammar.ident> <.Perlito5::Grammar::Space.opt_ws>
            [ '(' <paren_parse> ')'
              { $MATCH->{capture} = [ 'postfix_or_term',
                       'methcall',
                       Perlito5::AST::Var->new(
                               sigil       => '$',
                               namespace   => '',    # TODO - namespace
                               name        => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.ident"}),
                           ),
                       Perlito5::Match::flat($MATCH->{paren_parse}),
                     ]
              }
            | { $MATCH->{capture} = [ 'postfix_or_term',
                       'methcall_no_params',
                       Perlito5::AST::Var->new(
                               sigil       => '$',
                               namespace   => '',    # TODO - namespace
                               name        => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.ident"}),
                           ),
                      ]
              }
            ]


        | <Perlito5::Grammar.full_ident> <.Perlito5::Grammar::Space.opt_ws>
            [ '(' <paren_parse> ')'
              { $MATCH->{capture} = [ 'postfix_or_term',
                       'methcall',
                       Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.full_ident"}),   # TODO - split namespace
                       Perlito5::Match::flat($MATCH->{paren_parse}),
                     ]
              }
            | { $MATCH->{capture} = [ 'postfix_or_term',
                        'methcall_no_params',
                        Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.full_ident"})   # TODO - split namespace
                      ]
              }
            ]
        ]
};

token term_ternary {
    '?'  <ternary5_parse> ':'
                { $MATCH->{capture} = [ 'op',          '? :', Perlito5::Match::flat($MATCH->{ternary5_parse})  ] }
};

token term_paren {
    '('  <paren_parse>   ')'        { $MATCH->{capture} = [ 'postfix_or_term',  '( )',   Perlito5::Match::flat($MATCH->{paren_parse})   ] }
};

token term_square {
    '['  <square_parse>  ']'      { $MATCH->{capture} = [ 'postfix_or_term',  '[ ]',   Perlito5::Match::flat($MATCH->{square_parse})  ] }
};

token term_curly {
    '{'  <.Perlito5::Grammar::Space.ws>?
         <Perlito5::Grammar.exp_stmts> <.Perlito5::Grammar::Space.ws>?
    [ \} | { die 'Missing right curly or square bracket' } ]
    { $MATCH->{capture} = [ 'postfix_or_term', 'block', Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.exp_stmts"}) ] }
};


token declarator {
     'my' | 'state' | 'our' 
};

token term_declarator {
    <declarator> 
    [ <.Perlito5::Grammar::Space.ws> 
        [
          <Perlito5::Grammar::Block.named_sub>
          {
            my $sub = $MATCH->{"Perlito5::Grammar::Block.named_sub"}{capture};
            $sub->{decl} = Perlito5::Match::flat($MATCH->{declarator});
            $MATCH->{capture} = [ 'term', $sub ];
            return $MATCH;
          }
        | <Perlito5::Grammar.opt_type> 
        ]
    | ''
    ]
    <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar.var_ident>   # my Int $variable
    <Perlito5::Grammar::Attribute.opt_attribute>
        {
            my $decl = Perlito5::Match::flat($MATCH->{declarator});
            my $type = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar.opt_type"});

            die "No such class $type"
                if $type && ! $Perlito5::PACKAGES->{$type};

            my $var  = $MATCH->{"Perlito5::Grammar.var_ident"}{capture};
            $MATCH->{capture} = [ 'term', 
                Perlito5::AST::Decl->new(
                    decl => $decl,
                    type => $type,
                    var  => $var,
                    attributes => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Attribute.opt_attribute"}),
                ) ]
        }
};

token term_local {
    'local' <.Perlito5::Grammar::Space.opt_ws> <Perlito5::Grammar::Sigil.term_sigil>
        {
            my $decl = 'local';
            my $type = '';
            $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Sigil.term_sigil"})->[1];
            # hijack some string interpolation code to parse the possible subscript
            $MATCH = Perlito5::Grammar::String->double_quoted_var_with_subscript($MATCH);
            my $var = $MATCH->{capture};

            $MATCH->{capture} = [ 'term', 
                Perlito5::AST::Decl->new(
                    decl => $decl,
                    type => $type,
                    var  => $var
                ) ]
        }
};

token term_return {
    'return' <.Perlito5::Grammar::Space.opt_ws> <list_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{list_parse});
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'return',
                    arguments => $args eq '*undef*' ? [] : [$args],
                    namespace => ''
                 )
               ]
        }
};

token term_eval {
    # Note: this is eval-block; eval-string is parsed as a normal subroutine
    'eval' <.Perlito5::Grammar::Space.opt_ws> <before '{'> <term_curly>
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'eval',
                    arguments => [
                        Perlito5::AST::Do->new(
                            block => Perlito5::AST::Lit::Block->new( stmts => Perlito5::Match::flat($MATCH->{term_curly})->[2] ),
                        )
                    ], 
                    namespace => ''
                 )
               ]
        }
};


my $Expr_end_token = {
        ']' => 1,
        ')' => 1,
        '}' => 1,
        ';' => 1,
      
        'if' => 1,
      
        'for' => 1,
      
        'else' => 1,
        'when' => 1,
      
        'while' => 1,
        'until' => 1,
        'elsif' => 1,
      
        'unless' => 1,
      
        'foreach' => 1,
};
my $Expr_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];


my $List_end_token = { 
        ':' => 1,
      
        'or' => 1,
      
        'and' => 1,
        'xor' => 1,
        %$Expr_end_token,
};
my $List_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];


# end-tokens for named unary operators - used in "Grammar::Bareword" module
my $Argument_end_token = {
        ',' => 1,
        '<' => 1,   
        '>' => 1,   
        '=' => 1,   
        # '&' => 1,    '&' is not a delimiter - example: defined &$sub
        '|' => 1,   
        '^' => 1,   
        '?' => 1,   
      
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
     
        '...' => 1, 
        '<=>' => 1, 
        'cmp' => 1, 
        '<<=' => 1, 
        '>>=' => 1, 
        '||=' => 1, 
        '&&=' => 1, 
        '//=' => 1, 
        '**=' => 1, 
        %$List_end_token,
};
my $Argument_end_token_chars = [ 7, 6, 5, 4, 3, 2, 1 ];


sub argument_parse {
    my $self = $_[0];
    my $str = $_[1];
    my $pos = $_[2];
   
    # say "# argument_parse: input ",$str," at ",$pos;
    my $expr;
    my $last_pos = $pos;
    my $is_first_token = 1;
    my $lexer_stack = [];
    my $last_token_was_space = 1;
    my $get_token = sub {
        my $last_is_term = $_[0];
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Grammar::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
        }
        else {
            my $m = Perlito5::Grammar::Precedence->op_parse($str, $last_pos, $last_is_term);
            if ($m) {
                my $spc = Perlito5::Grammar::Space->ws($str, $m->{to});
                if ($spc) {
                    $m->{to} = $spc->{to};
                }
            }
            # say "# list lexer got: " . $m->perl;
            if (!$m) {
                return [ 'end', '*end*' ];
            }
            $v = $m->{capture};
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Grammar::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
            if ($v->[0] ne 'end') {
                $last_pos = $m->{to};
            }
        }
        # say "# list_lexer got " . $v->perl;
        # say "# list_lexer " . $v->perl;

        $last_token_was_space = ($v->[0] eq 'space');
        $is_first_token = 0;

        return $v;
    };
    my $prec = Perlito5::Grammar::Precedence->new(
        get_token       => $get_token, 
        reduce          => $reduce_to_ast,
        end_token       => $Argument_end_token,
        end_token_chars => $Argument_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# list_lexer return: ", $res->perl;
    if (scalar(@$res) == 0) {
        return {
            'str' => $str, 'from' => $pos, 'to' => $last_pos,
            capture => '*undef*' 
        };
    }
    my $result = pop_term($res);
    return {
        'str' => $str, 'from' => $pos, 'to' => $last_pos,
        capture => $result
    };
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
    my $last_token_was_space = 1;
    my $get_token = sub {
        my $last_is_term = $_[0];
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Grammar::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
        }
        else {
            my $m = Perlito5::Grammar::Precedence->op_parse($str, $last_pos, $last_is_term);
            if ($m) {
                my $spc = Perlito5::Grammar::Space->ws($str, $m->{to});
                if ($spc) {
                    $m->{to} = $spc->{to};
                }
            }
            # say "# list lexer got: " . $m->perl;
            if (!$m) {
                return [ 'end', '*end*' ];
            }
            $v = $m->{capture};
            if  (  $is_first_token
                && ($v->[0] eq 'op')
                && !(Perlito5::Grammar::Precedence::is_fixity_type('prefix', $v->[1]))
                )
            {
                # say "# finishing list - first token is: ", $v->[1];
                $v->[0] = 'end';
            }
            if ($v->[0] ne 'end') {
                $last_pos = $m->{to};
            }
        }
        # say "# list_lexer got " . $v->perl;

        # say "# list_lexer " . $v->perl;
        
        $last_token_was_space = ($v->[0] eq 'space');
        $is_first_token = 0;

        return $v;
    };
    my $prec = Perlito5::Grammar::Precedence->new(
        get_token       => $get_token, 
        reduce          => $reduce_to_ast,
        end_token       => $List_end_token,
        end_token_chars => $List_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# list_lexer return: ", $res->perl;
    if (scalar(@$res) == 0) {
        return {
            'str' => $str, 'from' => $pos, 'to' => $last_pos,
            capture => '*undef*'
        };
    }
    my $result = pop_term($res);
    return {
        'str' => $str, 'from' => $pos, 'to' => $last_pos,
        capture => $result
    };
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
        my $last_is_term = $_[0];
        my $m = Perlito5::Grammar::Precedence->op_parse($str, $last_pos, $last_is_term);
        if ($m) {
            my $spc = Perlito5::Grammar::Space->ws($str, $m->{to});
            if ($spc) {
                $m->{to} = $spc->{to};
            }
        }
        if (!$m) {
            my $msg = "Expected closing delimiter: $delimiter";
            $msg = 'Missing right curly or square bracket'
                if $delimiter eq '}' || $delimiter eq ']';
            die "$msg near ", $last_pos;
        }
        my $v = $m->{capture};
        if ($v->[0] ne 'end') {
            $last_pos = $m->{to};
        }
        # say "# circumfix_lexer " . $v->perl;
        return $v;
    };

    my %delim_token;
    $delim_token{ $delimiter } = 1;
    my $prec = Perlito5::Grammar::Precedence->new(
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
    return {
        'str' => $str, 'from' => $pos, 'to' => $last_pos, capture => $res
    };
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
    my $get_token = sub {
        my $last_is_term = $_[0];
        my $v;
        if (scalar(@$lexer_stack)) {
            $v = pop @$lexer_stack;
        }
        else {
            my $m = Perlito5::Grammar::Precedence->op_parse($str, $last_pos, $last_is_term);
            if ($m) {
                my $spc = Perlito5::Grammar::Space->ws($str, $m->{to});
                if ($spc) {
                    $m->{to} = $spc->{to};
                }
            }
            # say "# exp lexer got: " . $m->perl;
            if (!$m) {
                return [ 'end', '*end*' ];
            }
            $v = $m->{capture};
            if ($v->[0] ne 'end') {
                $last_pos = $m->{to};
            }
        }
        # say "# exp_lexer got " . $v->perl;
        return $v;
    };
    my $prec = Perlito5::Grammar::Precedence->new(
        get_token       => $get_token,
        reduce          => $reduce_to_ast,
        end_token       => $Expr_end_token,
        end_token_chars => $Expr_end_token_chars,
    );
    my $res = $prec->precedence_parse;
    # say "# exp terminated";
    if (scalar(@$res) == 0) {
        # say "# exp terminated with false";
        return 0;
    }
    my $result = pop_term($res);
    # say "# exp_parse result: ", $result->perl;
    return {
        'str' => $str, 'from' => $pos, 'to' => $last_pos,
        capture => $result
    };
}

1;

=begin

=head1 NAME

Perlito5::Grammar::Expression - Parser and AST generator for Perlito

=head1 SYNOPSIS

    Perlito5::Grammar::Expression->exp_parse($str)

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


package Perlito5::Grammar::Expression;

token term_pos {
    'pos' <.Perlito5::Grammar::Space::opt_ws>
    [
        <before '$'> <argument_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{argument_parse});
            $MATCH->{capture} = [ 'term',
                Perlito5::AST::Apply->new(
                    code      => "pos",
                    arguments => $args eq '*undef*' ? [ Perlito5::AST::Var::SCALAR_ARG() ] : [$args],
                )
            ];
        }
    |
        <!before '('>
         {
             $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'pos',
                    arguments => [ Perlito5::AST::Var::SCALAR_ARG() ],
                    bareword  => 1,
                 )
             ];
         }
    ]
};

token declarator {
     'my' | 'state' | 'our' 
};

token term_declarator {
    <declarator> 
    [ <.Perlito5::Grammar::Space::ws> 
        [
          <Perlito5::Grammar::Block::named_sub>
          {
            my $sub = $MATCH->{"Perlito5::Grammar::Block::named_sub"}{capture};
            $sub->{decl} = Perlito5::Match::flat($MATCH->{declarator});
            $MATCH->{capture} = [ 'term', $sub ];
            return $MATCH;
          }
        | <Perlito5::Grammar::opt_type> 
        ]
    | ''
    ]
    <.Perlito5::Grammar::Space::opt_ws> <Perlito5::Grammar::var_ident>   # my Int $variable
    <Perlito5::Grammar::Attribute::opt_attribute>
        {
            my $declarator = Perlito5::Match::flat($MATCH->{declarator});
            my $type = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::opt_type"});

            Perlito5::Compiler::error "No such class $type"
                if $type && ! $Perlito5::PACKAGES->{$type};

            my $var  = $MATCH->{"Perlito5::Grammar::var_ident"}{capture};
            Perlito5::Compiler::error "No package name allowed for variable $var->{sigil}$var->{name} in \"$declarator\""
                if $var->{namespace};
            $var->{_decl} = $declarator;
            $var->{_id}   = $Perlito5::ID++;
            $var->{_namespace} = $Perlito5::PKG_NAME if $declarator eq 'our';
            my $decl = Perlito5::AST::Decl->new(
                    decl => $declarator,
                    type => $type,
                    var  => $var,
                    attributes => Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Attribute::opt_attribute"}),
                );
            $MATCH->{capture} = [ 'term', $decl ];
        }
};

# these operators parse differently when followed by parenthesis
token operator_with_paren {
    'chomp' | 'chop' 
};

token term_operator_with_paren {
    <operator_with_paren> <.Perlito5::Grammar::Space::opt_ws> '('  <paren_parse>   ')'
        {
            $MATCH->{capture} = [ 'term', 
                Perlito5::AST::Apply->new(
                    code      => Perlito5::Match::flat($MATCH->{operator_with_paren}),
                    arguments => expand_list( Perlito5::Match::flat($MATCH->{paren_parse}) ),
                    namespace => '',
                ) ]
        }
};

token term_local {
    'local' <.Perlito5::Grammar::Space::opt_ws> <Perlito5::Grammar::Sigil::term_sigil>
        {
            my $declarator = 'local';
            my $type = '';
            $MATCH->{capture} = Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::Sigil::term_sigil"})->[1];
            # hijack some string interpolation code to parse the possible subscript
            $MATCH = Perlito5::Grammar::String::double_quoted_var_with_subscript($MATCH);
            my $var = $MATCH->{capture};

            my $look = Perlito5::Grammar::Scope::lookup_variable($var);
            if ( $look && ($look->{_decl} eq 'my' || $look->{_decl} eq 'state') ) {
                Perlito5::Compiler::error "Can\'t localize lexical variable $var->{sigil}$var->{name}";
            }
            # warn "look: ", Data::Dumper::Dumper($look)
            #     if ref($look) eq 'Perlito5::AST::Var';

            $var->{_id}   = $Perlito5::ID++;
            $var->{_decl} = $declarator;
            $var->{_namespace} = $Perlito5::PKG_NAME
                if !$var->{namespace} && !$var->{_namespace};
            my $decl = Perlito5::AST::Decl->new(
                    decl => $declarator,
                    type => $type,
                    var  => $var
                );
            $MATCH->{capture} = [ 'term', $decl ];
        }
};

token term_return {
    #        Unlike most named operators, this is also exempt from the
    #        looks-like-a-function rule, so "return ("foo")."bar"" will cause
    #        "bar" to be part of the argument to "return". See: perldoc -f return
    'return' <.Perlito5::Grammar::Space::opt_ws> <list_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{list_parse});
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'return',
                    arguments => $args eq '*undef*' ? [] : [$args],
                    namespace => '',
                    bareword  => $args eq '*undef*' ? 1 : 0,
                 )
               ]
        }
};

sub term_file_test {
    #        Unlike most named operators, this is also exempt from the
    #        looks-like-a-function rule, so "-X ("foo")."bar"" will cause
    #        "bar" to be part of the argument to "-X". See: perldoc -f -X
    my ($str, $pos) = @_;
    my $code = $str->[$pos] . $str->[$pos + 1];
    $pos += 2;
    my $spc = Perlito5::Grammar::Space::ws($str, $pos);
    if ($spc) {
        $pos = $spc->{to};
    }
    my $arg = argument_parse($str, $pos);
    if ($arg) {
        my $argument = Perlito5::Match::flat($arg);
        if (  ref($argument) eq 'Perlito5::AST::Apply'
           && (  $argument->{code} =~ /^prefix:<-\w>$/
              || $argument->{code} eq 'infix:<&&>'
              )
        ) {
            # stacked operators: "-f -w -x $file" is equivalent to "-x $file && -w _ && -f _"
            my $op2  = $argument;
            my $file = $op2->{arguments}[0];
            $arg->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => "infix:<&&>",
                    namespace => '',
                    arguments => [
                        $op2,
                        Perlito5::AST::Apply->new(
                            code      => "prefix:<$code>",
                            arguments => [
                                Perlito5::AST::Var->new(
                                    '_decl' => "global",
                                    '_namespace' => "main",
                                    'name' => "_",
                                    'namespace' => '',
                                    'sigil' => "*",
                                )
                            ],
                            namespace => '',
                            bareword  => 0,
                        ),
                    ],
                )
            ];
        }
        else {
            if ($argument eq '*undef*') {
                # If the argument is omitted, tests $_, except for "-t", which tests STDIN
                if ($code eq '-t') {
                    $argument = Perlito5::AST::Var->new(
                            '_decl' => "global",
                            '_namespace' => "main",
                            'name' => "STDIN",
                            'namespace' => '',
                            'sigil' => "*",
                        );
                }
                else {
                    $argument = Perlito5::AST::Var::SCALAR_ARG();
                }
            }
            $arg->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => "prefix:<$code>",
                    arguments => $argument eq '*undef*' ? [] : [$argument],
                    namespace => '',
                    bareword  => 0,
                 )
            ];
        }
    }
    return $arg;
};

token next_last_redo {
     'next' | 'last' | 'redo' | 'goto'
};
token term_next_last_redo {
    #        Unlike most named operators, this is also exempt from the
    #        looks-like-a-function rule, so "redo ("foo")."bar"" will cause
    #        "bar" to be part of the argument to "redo". See: perldoc -f redo
    <next_last_redo> <.Perlito5::Grammar::Space::opt_ws> <next_last_redo_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{next_last_redo_parse});
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => Perlito5::Match::flat($MATCH->{next_last_redo}),
                    arguments => $args eq '*undef*' ? [] : [$args],
                    namespace => '',
                    bareword  => $args eq '*undef*' ? 1 : 0,
                 )
               ]
        }
};

token term_scalar {
    'scalar' <.Perlito5::Grammar::Space::opt_ws> 
    [
        '('  <paren_parse>   ')'
        {
            my $args = Perlito5::Match::flat($MATCH->{paren_parse});

            Perlito5::Compiler::error "Not enough arguments for scalar"
                if $args eq '*undef*';

            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'scalar',
                    arguments => expand_list($args),
                    namespace => '',
                    bareword  => 0,
                 )
               ]
        }
    |
        <argument_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{argument_parse});
            my $op = Perlito5::Match::flat($MATCH->{unary_op});

            Perlito5::Compiler::error "Not enough arguments for scalar"
                if $args eq '*undef*';

            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'scalar',
                    arguments => $args eq '*undef*' ? [] : [$args],
                    namespace => '',
                    bareword  => $args eq '*undef*' ? 1 : 0,
                 )
               ]
        }
    ]
};


token unary_op {
     'shift' | 'pop'
};
token term_unary {
    <unary_op> <.Perlito5::Grammar::Space::opt_ws> 
    [
        '('  <paren_parse>   ')'
        {
            my $args = Perlito5::Match::flat($MATCH->{paren_parse});
            my $op = Perlito5::Match::flat($MATCH->{unary_op});

            Perlito5::Compiler::error "Not enough arguments for $op"
                if $op eq 'scalar' && $args eq '*undef*';

            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => $op,
                    arguments => expand_list($args),
                    namespace => '',
                    bareword  => 0,
                 )
               ]
        }
    |
        <before '->' >
        {
            my $op = Perlito5::Match::flat($MATCH->{unary_op});
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => $op,
                    arguments => [],
                    namespace => '',
                    bareword  => 1,
                 )
               ]
        }
    |
        <argument_parse>
        {
            my $args = Perlito5::Match::flat($MATCH->{argument_parse});
            my $op = Perlito5::Match::flat($MATCH->{unary_op});

            Perlito5::Compiler::error "Not enough arguments for $op"
                if $op eq 'scalar' && $args eq '*undef*';

            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => $op,
                    arguments => $args eq '*undef*' ? [] : [$args],
                    namespace => '',
                    bareword  => $args eq '*undef*' ? 1 : 0,
                 )
               ]
        }
    ]
};


token term_eval {
    # Note: this is eval-block; eval-string is parsed as a normal subroutine
    'eval' <Perlito5::Grammar::block>
        {
            $MATCH->{capture} = [ 'term',
                 Perlito5::AST::Apply->new(
                    code      => 'eval',
                    arguments => [
                        Perlito5::Match::flat($MATCH->{"Perlito5::Grammar::block"}),
                    ], 
                    namespace => ''
                 )
               ]
        }
};

token term_not {
    'not' <.Perlito5::Grammar::Space::opt_ws> '('  <paren_parse>   ')'
        {
            $MATCH->{capture} = [ 'term', 
                Perlito5::AST::Apply->new(
                    code      => 'prefix:<not>',
                    arguments => expand_list( Perlito5::Match::flat($MATCH->{paren_parse}) ),
                    namespace => '',
                ) ]
        }
};

sub term_core {
    my $str  = shift;
    my $pos  = shift;

    # CORE::print, CORE::tr

    my $tok = join( "", @{$str}[ $pos .. $pos + 20 ] );

    return if length($tok) < 7;
    return if substr($tok, 0, 6) ne 'CORE::';
    return if substr($tok, 6, 1) !~ /\w/;

    $tok = substr($tok, 6);
    $pos += 6;

    package Perlito5::Grammar::Precedence;
    for my $len ( @Term_chars ) {
        my $term = substr($tok, 0, $len);
        if (exists($Term{$term})) {
            my $c1 = $str->[$pos + $len - 1];
            my $c2 = $str->[$pos + $len];
            if ( is_num($c1) || !is_ident_middle($c1) || !is_ident_middle($c2) ) {
                my $m = $Term{$term}->($str, $pos);
                if ($m) {
                    my $node = $m->{capture}[1];
                    $node->{namespace} = 'CORE';
                    # warn "term_core: ", Perlito5::Dumper::Dumper( $m );
                    return $m;
                }
            }
        }
    }

    return;
};

Perlito5::Grammar::Precedence::add_term( 'my'     => \&term_declarator );
Perlito5::Grammar::Precedence::add_term( 'our'    => \&term_declarator );
Perlito5::Grammar::Precedence::add_term( 'eval'   => \&term_eval );
Perlito5::Grammar::Precedence::add_term( 'state'  => \&term_declarator );
Perlito5::Grammar::Precedence::add_term( 'local'  => \&term_local );
Perlito5::Grammar::Precedence::add_term( 'return' => \&term_return );
Perlito5::Grammar::Precedence::add_term( 'pos'    => \&term_pos );
Perlito5::Grammar::Precedence::add_term( 'chomp'  => \&term_operator_with_paren );
Perlito5::Grammar::Precedence::add_term( 'chop'   => \&term_operator_with_paren );
Perlito5::Grammar::Precedence::add_term( 'next'   => \&term_next_last_redo );
Perlito5::Grammar::Precedence::add_term( 'last'   => \&term_next_last_redo );
Perlito5::Grammar::Precedence::add_term( 'redo'   => \&term_next_last_redo );
Perlito5::Grammar::Precedence::add_term( 'shift'  => \&term_unary );
Perlito5::Grammar::Precedence::add_term( 'pop'    => \&term_unary );
Perlito5::Grammar::Precedence::add_term( 'scalar' => \&term_scalar );
Perlito5::Grammar::Precedence::add_term( 'not'    => \&term_not );

Perlito5::Grammar::Precedence::add_term( $_ => \&term_file_test )
    for qw( -r -w -x -o -R -W -X -O -e -z -s -f -d -l -p -S -b -c -t -u -g -k -T -B -M -A -C );

Perlito5::Grammar::Precedence::add_term( 'CORE'  => \&term_core );

1;


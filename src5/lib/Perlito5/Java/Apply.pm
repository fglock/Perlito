use Perlito5::DumpToAST;

package Perlito5::AST::Apply;
{
    use strict;

    sub emit_qr_java {
        my ($regex, $modifier, $level) = @_;
        if ( $modifier eq '' && ref( $regex ) eq "Perlito5::AST::Var" && $regex->{sigil} eq '$' ) {
            # return as-is because var may contain a qr//
            return $regex->emit_java($level);
        }
        my %flags = map { $_ => 1 } split //, $modifier;
        # warn Perlito5::Dumper::Dumper(\%flags);
        my $flag_string = join( " | ", 
            ( $flags{'i'} ? 'Pattern.CASE_INSENSITIVE' : () ),
            ( $flags{'x'} ? 'Pattern.COMMENTS'         : () ),
            ( $flags{'m'} ? 'Pattern.MULTILINE'        : () ),
            ( $flags{'s'} ? 'Pattern.DOTALL'           : () ),
        ) || '0';

        my $flag_xx = 'false';
        $flag_xx = 'true' if $modifier =~ /xx/;     # Perl 5.26 "/xx"

        my $s = 'new PlRegex(' . Perlito5::Java::to_str( $regex ) . ', ' . $flag_string . ', ' . $flag_xx . ')';
        if ( ref( $regex ) eq "Perlito5::AST::Buf" ) {
            # precompile regex
            my $label = Perlito5::Java::get_label();
            push @Perlito5::Java::Java_constants, "public static final PlRegex $label = $s;";
            return $label;
        }
        return $s;
    }

    sub emit_regex_java {
        my $op = shift;
        my $var = shift;
        my $regex = shift;
        my $level     = shift;
        my $wantarray = shift;

        $regex = Perlito5::Macro::preprocess_regex($regex);

        my $str;
        my $code = $regex->{code};
        my $regex_args = $regex->{arguments};
        my $modifier_global = 'false';
        if ($code eq 'p5:s') {
            my $replace = $regex_args->[1];
            my $modifier = $regex_args->[2]->{buf};
            my $replace_java;
            if (ref($replace) eq 'Perlito5::AST::Buf') {
                $replace_java = $replace->{buf};
                # $replace_java =~ s{\\}{\\\\}g;
                $replace_java = Perlito5::Java::escape_string($replace_java);
            }
            else {
                if (ref($replace) ne 'Perlito5::AST::Block') {
                    $replace = Perlito5::AST::Block->new(
                        stmts => [ $replace ],
                    );
                }
                $replace_java = Perlito5::AST::Sub->new(
                    'block' => $replace,
                    'attributes' => [],
                    _do_block => 1,
                )->emit_java($level);
                $modifier =~ s/e//g;
            }
            if ($modifier =~ /g/) {
                $modifier_global = 'true';
                $modifier =~ s/g//g;
            }
            $str = 'PerlOp.replace('
                    . $var->emit_java($level, 'scalar') . ', '
                    . emit_qr_java( $regex_args->[0], $modifier, $level ) . ', '
                    . $replace_java . ', '
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . $modifier_global
                  . ")";
        }
        elsif ($code eq 'p5:m') {
            my $modifier = $regex_args->[1]->{buf};
            if ($modifier =~ /g/) {
                $modifier_global = 'true';
                $modifier =~ s/g//g;
            }
            my $modifier_c = 'false';
            if ($modifier =~ /c/) {
                $modifier_c = 'true';
                $modifier =~ s/c//g;
            }
            $str = 'PerlOp.match('
                    . $var->emit_java($level) . ', '
                    . emit_qr_java( $regex_args->[0], $modifier, $level ) . ', '
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . $modifier_global . ', '
                    . $modifier_c
                  . ")";
        }
        elsif ($code eq 'p5:tr') {
            my $search  = $regex_args->[0];
            my $replace = $regex_args->[1];
            for my $node ($search, $replace) {
                if (ref($node) eq 'Perlito5::AST::Buf') {
                    $node->{buf} = Perlito5::Regex::expand_character_range($node->{buf});
                }
            }
            $str = "PerlOp.tr("
                    . $var->emit_java($level) . ', '
                    . $regex_args->[0]->emit_java($level) . ', '
                    . $regex_args->[1]->emit_java($level) . ', '
                    . Perlito5::Java::escape_string($regex_args->[2]->{buf}) . ', '
                    . Perlito5::Java::to_context($wantarray)
                  . ")",
        }
        else {
            die "Error: regex emitter - unknown operator $code";
        }

        if ($op eq '=~') {
            return $str;
        }
        if ($op eq '!~') {
            return 'new PlBool(!(' . $str . '.to_boolean()))'
        }
        die "Error: regex emitter";
    }

    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $code = $self->{code};

        if ( $code eq 'vec' ) {
            # vec($i,  0, 32) = 0x5065726C;
            # public PlObject vecSet(PlObject pOffset, PlObject pBits, PlObject pValue) {
            return $self->{arguments}[0]->emit_java($level, 'scalar', 'lvalue') . '.vecSet('
            .       $self->{arguments}[1]->emit_java($level, 'scalar') . ', '
            .       $self->{arguments}[2]->emit_java($level, 'scalar') . ', '
            .       $arguments->emit_java($level, 'scalar')
            . ')';
        }

        if ( $code eq 'circumfix:<( )>' ) {
            # my ($x, $y) = ...
            # local ($x, $y) = ...
            # ($x, $y) = ...
            return 'PlArray.static_list_set('
                . join( ', ',
                    Perlito5::Java::to_context($wantarray),
                    Perlito5::Java::to_list([$arguments], $level),
                    map( $_->emit_java( $level, 'list', 'lvalue' ), @{ $self->{arguments} } ),
                )
            . ')'
        }
        if ($code eq 'pos') {
            my @lvalue = @{$self->{arguments}};
            if (!@lvalue) {
                push @lvalue, Perlito5::AST::Var::SCALAR_ARG();
            }
            return 'PerlOp.set_pos('
             .      $lvalue[0]->emit_java($level, 'scalar') . ', '
             .      $arguments->emit_java($level, 'scalar')
             . ')';
        }
        if ($code eq 'prefix:<$#>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'array' ) . '.array_deref('
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME)
                . ').set_end_of_array_index('
                . Perlito5::Java::to_scalar([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<$>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . '.scalar_deref_set('
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', '
                . Perlito5::Java::to_scalar([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<@>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'array' ) . '.array_deref_set('
                . Perlito5::Java::to_list([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<%>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'hash' ) . '.hash_deref_set('
                . Perlito5::Java::to_list([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<*>') {
            return 'PlV.glob_set(' 
                . Perlito5::Java::to_scalar($self->{arguments}, $level+1) . ', '
                . Perlito5::Java::to_scalar([$arguments], $level+1) . ', '
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        if ($code eq 'ternary:<? :>') {
            return $self->emit_java( $level+1 ) . '.set(' . $arguments->emit_java( $level+1 ) . ')';
        }
        if ($code eq 'substr') {
            return $self->emit_java( $level+1, 'scalar', 'lvalue' ) . '.set(' . $arguments->emit_java( $level+1 ) . ')';
        }

        my $open  = $wantarray eq 'void' || $wantarray eq 'statement' ? '' : '(';
        my $close = $wantarray eq 'void' || $wantarray eq 'statement' ? '' : ')';
        $open . $self->emit_java( $level+1 ) . ' = ' . $arguments->emit_java( $level+1 ) . $close;
    }

    my %emit_js = (
        'infix:<=~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'infix:<!~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '!~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'p5:s' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:m' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[2], $self, $level, $wantarray );
        },
        'p5:tr' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:qr' => sub {
            my ($self, $level, $wantarray) = @_;
            return emit_qr_java($self->{arguments}[0], $self->{arguments}[1]{buf}, $level);
        },
        '__PACKAGE__' => sub {
            my ($self, $level, $wantarray) = @_;
            return Perlito5::AST::Buf->new( buf => $Perlito5::PKG_NAME )->emit_java($level, 'scalar');
        },
        '__SUB__' => sub {
            my ($self, $level, $wantarray) = @_;
            'this.getCurrentSub()'
        },
        'wantarray' => sub {
            my ($self, $level, $wantarray) = @_;
            '(return_context == PlCx.VOID ? PlCx.UNDEF : return_context == PlCx.SCALAR ? PlCx.EMPTY : PlCx.INT1)';
        },
        'uc' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlString('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().toUpperCase())'
        },
        'lc' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlString('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().toLowerCase())'
        },
        'index' => sub {
            my ($self, $level, $wantarray) = @_;
            if($self->{arguments}->[2]) {
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.index('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ', '
                . $self->{arguments}->[2]->emit_java($level, 'scalar') . ')'
            }
            else {
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.index('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
            }
        },
        'rindex' => sub {
            my ($self, $level, $wantarray) = @_;
            if($self->{arguments}->[2]) {
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.rindex('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ', '
                . $self->{arguments}->[2]->emit_java($level, 'scalar') . ')'
            }
            else {
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.rindex('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
            }
        },
        'ord' => sub {
            my ($self, $level, $wantarray) = @_;
            'PerlOp.ord(' . Perlito5::Java::to_str($self->{arguments}->[0], $level) . ')'
        },
        'chr' => sub {
            my ($self, $level, $wantarray) = @_;

            # this is necessary to support characters with code > 65535
            # new String(Character.toChars((int)(1114109L)))

              'new PlString(new String(Character.toChars('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_int())))'
        },
        'vec' => sub {
            my ($self, $level, $wantarray) = @_;
            # vec($i,  0, 32)
              $self->{arguments}[0]->emit_java($level, 'scalar') . '.vec('
            .       $self->{arguments}[1]->emit_java($level, 'scalar') . ', '
            .       $self->{arguments}[2]->emit_java($level, 'scalar')
            . ')';
        },
        'rand' => sub {
            my ($self, $level, $wantarray) = @_;
              'PerlOp.rand('
            . ( $self->{arguments}->[0]
              ? $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_double()'
              : '1.0'
              )
            . ')'
        },
        'srand' => sub {
            my ($self, $level, $wantarray) = @_;
              'PerlOp.srand('
            . ( $self->{arguments}->[0]
              ? $self->{arguments}->[0]->emit_java($level, 'scalar')
              : ''
              )
            . ')'
        },
        ( map {
                my $op = $_;
                ( $op => sub {
                        my ($self, $level, $wantarray) = @_;
                        $self->{arguments}->[0]->emit_java($level, 'scalar') . '.' . $op . '()'
                      }
                )
            }
            qw/ abs sqrt cos sin exp log ucfirst lcfirst quotemeta /
        ),
        ( map {
                my $op = $_;
                ( $op => sub {
                        my ($self, $level, $wantarray) = @_;
                        $self->{arguments}->[0]->emit_java($level, 'scalar') . '.op_' . $op . '()'
                      }
                )
            }
            qw/ int /
        ),
        ( map {
                my ($op, $java_op, $native_op) = @$_;
                ( $op => sub {
                        my ($self, $level, $wantarray) = @_;

                        my $op1 = $self->{arguments}->[0]->emit_java($level, 'scalar');
                        my $op2 = $self->{arguments}->[1]->emit_java($level, 'scalar');

                        if ($self->{_integer} && $native_op) {
                            return 'new PlInt('
                            . '(' . $op1 . '.to_long() ' . $native_op . ' ' . $op2 . '.to_long()' . ')'
                            . ' & 4294967295L'
                            . ')'
                        }

                        $op1 . '.' . $java_op . '(' . $op2 . ')'
                      }
                )
            } (
              [ 'infix:<%>',   'mod',    '%'  ],
              [ 'infix:<>>>',  'int_shr' ],
              [ 'infix:<<<>',  'int_shl' ],
              [ 'infix:<^>',   'int_xor' ],
              [ 'infix:<&>',   'int_and' ],
              [ 'infix:<|>',   'int_or'  ],
              [ 'infix:<+>',   'add',    '+'  ],
              [ 'infix:<->',   'sub',    '-'  ],
              [ 'infix:<*>',   'mul',    '*'  ],
              [ 'infix:</>',   'div',    '/'  ],
              [ 'infix:<==>',  'num_eq', '==' ],
              [ 'infix:<!=>',  'num_ne', '!=' ],
              [ 'infix:<>>',   'num_gt', '>'  ],
              [ 'infix:<>=>',  'num_ge', '>=' ],
              [ 'infix:<<>',   'num_lt', '<'  ],
              [ 'infix:<<=>',  'num_le', '<=' ],
              [ 'infix:<eq>',  'str_eq'  ],
              [ 'infix:<ne>',  'str_ne'  ],
              [ 'infix:<gt>',  'str_gt'  ],
              [ 'infix:<ge>',  'str_ge'  ],
              [ 'infix:<lt>',  'str_lt'  ],
              [ 'infix:<le>',  'str_le'  ],
              [ 'infix:<cmp>', 'str_cmp' ],
              [ 'infix:<<=>>', 'num_cmp' ],
              [ 'infix:<**>',  'pow'     ],
              [ 'atan2',       'atan2'   ],
              [ 'infix:<^.>',  'str_xor' ],
              [ 'infix:<&.>',  'str_and' ],
              [ 'infix:<|.>',  'str_or'  ],
            )
        ),
        ( map {
                my ($op, $java_op, $native_op) = @$_;
                ( $op => sub {
                        my ($self, $level, $wantarray) = @_;

                        my $op  = $self->{arguments}->[0]->emit_java($level, 'scalar', 'lvalue');
                        my $op1 = $self->{arguments}->[0]->emit_java($level, 'scalar');
                        my $op2 = $self->{arguments}->[1]->emit_java($level, 'scalar');

                        if ($self->{_integer} && $native_op) {
                            return 
                              $op . '.set('
                            .   'new PlInt('
                            .       '(' . $op1 . '.to_long() ' . $native_op . ' ' . $op2 . '.to_long()' . ')'
                            .       ' & 4294967295L'
                            .   ')'
                            . ')'
                        }

                        $op . '.' . $java_op . '(' . $op2 . ')'
                      }
                )
            } (
              [ 'infix:<%=>',   'self_assign_mod',    '%'  ],
              [ 'infix:<>>=>',  'self_assign_int_shr' ],
              [ 'infix:<<<=>',  'self_assign_int_shl' ],
              [ 'infix:<^=>',   'self_assign_int_xor' ],
              [ 'infix:<&=>',   'self_assign_int_and' ],
              [ 'infix:<|=>',   'self_assign_int_or' ],
              [ 'infix:<+=>',   'self_assign_add',    '+'  ],
              [ 'infix:<-=>',   'self_assign_sub',    '-'  ],
              [ 'infix:<*=>',   'self_assign_mul',    '*'  ],
              [ 'infix:</=>',   'self_assign_div',    '/'  ],
              [ 'infix:<**=>',  'self_assign_pow'     ],
              [ 'infix:<x=>',   'self_assign_string_replicate' ],
              [ 'infix:<||=>',  'self_assign_or' ],
              [ 'infix:<&&=>',  'self_assign_and' ],
              [ 'infix:<^.=>',  'self_assign_str_xor' ],
              [ 'infix:<&.=>',  'self_assign_str_and' ],
              [ 'infix:<|.=>',  'self_assign_str_or' ],
            )
        ),
        'infix:<~~>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg0 = $self->{arguments}->[0];
            my $arg1 = $self->{arguments}->[1];
            # TODO - test argument type
            #   See: http://perldoc.perl.org/perlop.html#Smartmatch-Operator
            # if (Perlito5::Java::is_num($arg1)) {
            #     # ==
            # }
            'PerlOp.smartmatch_scalar('
                . $arg0->emit_java($level, 'scalar') . ', '
                . $arg1->emit_java($level, 'scalar') . ')'
        },

        'infix:<&&>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'statement') {
                return
                    Perlito5::AST::If->new(
                        cond => $self->{arguments}[0],
                        body => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[1] ] ),
                        otherwise => Perlito5::AST::Block->new( stmts => [] ),
                    )->emit_java($level, $wantarray);
            }
            # and1(x) ? y : and3()
            '(PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PerlOp.and3())'
        },
        'infix:<and>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'statement') {
                return
                    Perlito5::AST::If->new(
                        cond => $self->{arguments}[0],
                        body => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[1] ] ),
                        otherwise => Perlito5::AST::Block->new( stmts => [] ),
                    )->emit_java($level, $wantarray);
            }
            # and1(x) ? y : and3()
            '(PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PerlOp.and3())'
        },
        'infix:<||>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'statement') {
                return
                    Perlito5::AST::If->new(
                        cond => $self->{arguments}[0],
                        body => Perlito5::AST::Block->new( stmts => [] ),
                        otherwise => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[1] ] ),
                    )->emit_java($level, $wantarray);
            }
            # or1(x) ? or2() : y
            '(PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')'
        },
        'infix:<or>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'statement') {
                return
                    Perlito5::AST::If->new(
                        cond => $self->{arguments}[0],
                        body => Perlito5::AST::Block->new( stmts => [] ),
                        otherwise => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[1] ] ),
                    )->emit_java($level, $wantarray);
            }
            # or1(x) ? or2() : y
            '(PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')'
        },
        'infix:<//>' => sub { 
            my ($self, $level, $wantarray) = @_;
            # defined_or1(x) ? defined_or2() : y
            '(PerlOp.defined_or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.defined_or2() : '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')'
        },
        'infix:<xor>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::Java::to_boolean( $self->{arguments}->[0], $level ) . ' ? new PlBool(!'
                 . Perlito5::Java::to_boolean($self->{arguments}->[1], $level) . ') : '
                 . ( $self->{arguments}->[1] )->emit_java( $level, $wantarray ) . ')';
        },
        'infix:<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
              Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_java($level)  . ', ' 
            . $self->{arguments}[1]->emit_java($level)
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            'new PlBool(!(' . Perlito5::Java::to_boolean( $self->{arguments}->[0], $level ) . '))';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $arg = pop(@{$self->{arguments}});
            if (!$arg) {
                return 'PlCx.TRUE';
            }
            'new PlBool(!( ' . Perlito5::Java::to_boolean( $arg, $level ) . '))';
        },
        'prefix:<~>' => sub {
            my $self  = shift;
            my $level = shift;
            my $arg = $self->{arguments}->[0];
            $arg->emit_java( $level, 'scalar' ) . '.complement()';
        },
        'prefix:<~.>' => sub {
            my $self  = shift;
            my $level = shift;
            my $arg = $self->{arguments}->[0];
            $arg->emit_java( $level, 'scalar' ) . '.str_complement()';
        },
        'prefix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa('Perlito5::AST::Int')) {
                $arg = Perlito5::AST::Int->new( int => -$arg->{int} );
                return $arg->emit_java( $level, 'scalar' );
            }
            if ($arg->isa('Perlito5::AST::Num')) {
                $arg = Perlito5::AST::Num->new( num => -$arg->{num} );
                return $arg->emit_java( $level, 'scalar' );
            }
            # negation of bareword treated like string
            if ($arg->isa('Perlito5::AST::Apply') && $arg->{bareword}) {
                $arg = Perlito5::AST::Buf->new( buf => $arg->{code} );
            }
            $arg->emit_java( $level, 'scalar' ) . '.neg()';
        },
        'prefix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
            $self->{arguments}->[0]->emit_java( $level, $wantarray );
        },
        'require' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            if ($arg->{is_version_string}) {
                # require VERSION
                # create a PlStringConstant
                my $code = Perlito5::AST::Buf->new( buf => "Perlito5::test_perl_version" )->emit_java($level, 'scalar');
                return $code . '.apply(PlCx.VOID, new PlArray('
                    .       $arg->emit_java( $level, 'scalar' )
                    . '))';
            }
            # require FILE
            'PlCORE.require('
                . Perlito5::Java::to_context($wantarray) . ', '
                . Perlito5::Java::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 'true' : 'false') 
            . ')';
        },
        'prefix:<$>' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my $arg  = $self->{arguments}->[0];
            if ($autovivification_type eq 'lvalue') {
                return $arg->emit_java( $level, 'scalar', 'lvalue' ) . '.scalar_deref_lvalue('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
            }
            elsif ( $self->{_strict_refs} ) {
                return $arg->emit_java( $level, 'scalar', 'scalar' ) . '.scalar_deref_strict()';
            }
            return $arg->emit_java( $level, 'scalar', 'scalar' ) . '.scalar_deref('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
        },
        'prefix:<@>' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my $arg   = $self->{arguments}->[0];
            my $s;
            if ($autovivification_type eq 'lvalue') {
                $s = Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref_lvalue()';
            }
            elsif ( $self->{_strict_refs} ) {
                $s = Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref_strict()';
            }
            else {
                $s = Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
            }
            return $wantarray eq 'scalar'
                ? "$s.scalar()"
                : $s;
        },
        'prefix:<$#>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( $self->{_strict_refs} ) {
                return  Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref_strict().end_of_array_index()';
            }
            return  Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ').end_of_array_index()';
        },
        'prefix:<%>' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my $arg   = $self->{arguments}->[0];
            if ($autovivification_type eq 'lvalue') {
                return Perlito5::Java::emit_java_autovivify( $arg, $level, 'hash' ) . '.hash_deref('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
            }
            elsif ( $self->{_strict_refs} ) {
                return Perlito5::Java::emit_java_autovivify( $arg, $level, 'hash' ) . '.hash_deref_strict()';
            }
            return Perlito5::Java::emit_java_autovivify( $arg, $level, 'hash' ) . '.hash_deref('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
        },
        'prefix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $invocant = $self->{arguments}->[0]->emit_java($level);
            if ( !$self->{_strict_refs} ) {
                $invocant = 'PlV.code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $invocant . ')';
            }
            return $invocant . '.apply('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . 'List__'
                . ')';
        },
        'prefix:<*>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return Perlito5::Java::to_filehandle($arg, $level+1);
        },
        'circumfix:<[ ]>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'new PlArrayRef(new PlArray(' . Perlito5::Java::to_list_for_push( $self->{arguments}, $level ) . '))';
        },
        'circumfix:<{ }>' => sub {
            my ($self, $level, $wantarray) = @_;
            'new PlHashRef(new PlHash(' . Perlito5::Java::to_list_for_push( $self->{arguments}, $level ) . '))';
        },
        'prefix:<\\>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Apply') ) {
                if ( $arg->{code} eq 'prefix:<@>' ) {
                    return 'new PlArrayRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->{code} eq 'prefix:<%>' ) {
                    return 'new PlHashRef(' . $arg->emit_java($level) . ')';
                }
                # if ( $arg->{code} eq '*' ) {
                #     # TODO
                #     return '(new PlGlobRef(' . $arg->emit_java($level) . '))';
                # }
                if ( $arg->{code} eq 'circumfix:<( )>' ) {
                    # \( @x )
                    return 'PlArray.construct_list_of_references(' . Perlito5::Java::to_list( $arg->{arguments}, $level ) . ')';
                }
                if ( $arg->{code} eq 'prefix:<&>' ) {
                    return 'PlV.code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->{arguments}->[0]->emit_java($level) . ')';
                }
            }
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return 'new PlArrayRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->sigil eq '%' ) {
                    return 'new PlHashRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->sigil eq '*' ) {
                    return 'new PlGlobRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->sigil eq '&' ) {
                    my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                    return 'PlV.cget(' . Perlito5::Java::escape_string($namespace . '::' . $arg->{name} ) . ')'
                }
            }
            return 'PlV.make_reference(' . $arg->emit_java($level) . ')';
        },
        'postfix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return Perlito5::Java::to_native_num($arg, $level, $wantarray) . '++';
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.post_incr()'
        },
        'postfix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return Perlito5::Java::to_native_num($arg, $level, $wantarray) . '--';
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.post_decr()'
        },
        'prefix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return '++' . Perlito5::Java::to_native_num($arg, $level, $wantarray);
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.pre_incr()'
        },
        'prefix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return '--' . Perlito5::Java::to_native_num($arg, $level, $wantarray);
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.pre_decr()'
        },

        'infix:<x>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if (  ref($arg) eq 'Perlito5::AST::Apply'
               && ( $arg->{code} eq 'circumfix:<( )>' || $arg->{code} eq 'list:<,>' )
               )
            {
                # ($v) x $i
                # qw( 1 2 3 ) x $i
                return 'PerlOp.list_replicate('
                           . Perlito5::Java::to_list( [$self->{arguments}->[0] ], $level) . ', '
                           . $self->{arguments}->[1]->emit_java($level, 'scalar') . ', '
                           . Perlito5::Java::to_context($wantarray)
                        . ')'
            }
              Perlito5::Java::to_str($self->{arguments}->[0], $level) . '.string_replicate('
            .       $self->{arguments}->[1]->emit_java($level, 'scalar')
            . ')'
        },

        'list:<.>' => sub {
            my ($self, $level, $wantarray) = @_;
            'new PlString(' . join( ' + ', map( Perlito5::Java::to_native_str($_, $level, 'scalar'), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Java::to_list( $self->{arguments}, $level );
        },
        'infix:<..>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'new PerlRange('
                              . $self->{arguments}->[0]->emit_java($level) . ', '
                              . $self->{arguments}->[1]->emit_java($level)
                        . ').range('
                              . Perlito5::Java::to_context($wantarray) . ', '
                              . '"' . Perlito5::Java::get_label() . '"' . ', '
                              . '0'
                        . ')'
        },
        'infix:<...>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'new PerlRange('
                              . $self->{arguments}->[0]->emit_java($level) . ', '
                              . $self->{arguments}->[1]->emit_java($level)
                        . ').range('
                              . Perlito5::Java::to_context($wantarray) . ', '
                              . '"' . Perlito5::Java::get_label() . '"' . ', '
                              . '1'
                        . ')'
        },
        'delete' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->{_real_sigil} eq '%'
                   )
                {
                    $v = Perlito5::AST::Var->new(%$v, sigil => '%');

                    if (!defined($v->{name})) {
                        # delete $Module::{foo}
                        my $index = Perlito5::Java::escape_string($v->{namespace} . '::');
                        return "PerlOp.deleteSymbolTable(" . $index . ', ' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                    }

                    return $v->emit_java($level) . '.hdelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
                if (  $v->isa('Perlito5::AST::Apply')
                   && $v->{code} eq 'prefix:<$>'
                   )
                {
                    # $$a{0} ==> $a->{0}
                    $arg = Perlito5::AST::Call->new(
                        'method' => 'postcircumfix:<{ }>',
                        'invocant' => $v->{arguments}[0],
                        'arguments' => $arg->{index_exp},
                    );
                }
                else {
                    return $v->emit_java($level, $wantarray, 'hash') . '.hdelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->{_real_sigil} eq '@'
                   )
                {
                    $v = Perlito5::AST::Var->new(%$v, sigil => '@');
                    return $v->emit_java($level) . '.adelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->{index_exp}->emit_java($level) . ')';
                }
                if (  $v->isa('Perlito5::AST::Apply')
                   && $v->{code} eq 'prefix:<$>'
                   )
                {
                    # $$a[0] ==> $a->[0]
                    $arg = Perlito5::AST::Call->new(
                        'method' => 'postcircumfix:<[ ]>',
                        'invocant' => $v->{arguments}[0],
                        'arguments' => $arg->{index_exp},
                    );
                }
                else {
                    return $v->emit_java($level, $wantarray, 'array') . '.adelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->{index_exp}->emit_java($level) . ')';
                }
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.hdelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_java($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.adelete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->{arguments}->emit_java($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                die 'TODO delete &code';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                die 'TODO delete &$code';
            }
        },
        'ternary:<? :>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'statement') {
                return
                    Perlito5::AST::If->new(
                        cond => $self->{arguments}[0],
                        body => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[1] ] ),
                        otherwise => Perlito5::AST::Block->new( stmts => [ $self->{arguments}[2] ] ),
                    )->emit_java($level, $wantarray);
            }
               '( ' . Perlito5::Java::to_boolean( $self->{arguments}->[0], $level )
            . ' ? ' . ( $self->{arguments}->[1] )->emit_java( $level, $wantarray )
            . ' : ' . ( $self->{arguments}->[2] )->emit_java( $level, $wantarray )
            .  ')';
        },
        'scalar' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my @args = @{$self->{arguments}};
            my $arg = pop @args;
            my @out;
            if ($arg) {
                for my $arg (@args) {
                    push @out, $arg->emit_java( $level, 'void' );
                }
                push @out, $arg->emit_java( $level, 'scalar', $autovivification_type );
            }
            return $out[0] if @out == 1;
            return 'PerlOp.context('
            . join( ', ',
                    Perlito5::Java::to_context('scalar'),
                    @out )
            . ')';
        },
        'circumfix:<( )>' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my @args = @{$self->{arguments}};
            my $arg = pop @args;
            my @out;
            if ($arg) {
                for my $arg (@args) {
                    my $context = $wantarray eq 'list' ? 'list' : 'void';
                    push @out, $arg->emit_java( $level, $context );
                }
                push @out, $arg->emit_java( $level, $wantarray, $autovivification_type );
            }
            return $out[0] if @out == 1;
            return 'PerlOp.context('
            . join( ', ',
                    Perlito5::Java::to_context($wantarray),
                    @out )
            . ')';
        },
        'infix:<=>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];
            return $parameters->emit_java_set($arguments, $level+1, $wantarray);
        },

        'readpipe' => sub {
            my ($self, $level, $wantarray) = @_;
            die "TODO - readpipe() not implemented";
        },
        'waitpid' => sub {
            my ($self, $level, $wantarray) = @_;
            die "TODO - waitpid() not implemented";
        },
        'glob' => sub {
            my ($self, $level, $wantarray) = @_;
            die "TODO - glob() not implemented";
        },
        'break' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            die "TODO - break() not implemented";
        },
        'next' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            if ($label == 0) {
                return 'PerlOp.next()';
            }
            'PerlOp.next(' . $label . ')';
        },
        'last' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            if ($label == 0) {
                return 'PerlOp.last()';
            }
            'PerlOp.last(' . $label . ')';
        },
        'redo' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            'PerlOp.redo(' . $label . ')';
        },
        'return' => sub {
            my ($self, $level, $wantarray) = @_;

            if (($wantarray eq 'void' || $wantarray eq 'statement') && $Perlito5::JAVA_CAN_RETURN) {
                my $has_local = $Perlito5::JAVA_HAS_LOCAL;
                my $local_label = $Perlito5::JAVA_LOCAL_LABEL;
                if (!@{$self->{arguments}}) {
                    return Perlito5::Java::LexicalBlock::emit_return($has_local, $local_label, 'PerlOp.context(want)'); 
                }
                else {
                    return Perlito5::Java::LexicalBlock::emit_return($has_local, $local_label,
                        Perlito5::Java::to_runtime_context( $self->{arguments}, $level+1, 'return' )
                    );
                }
            }
 
            $Perlito5::THROW_RETURN = 1;

            $wantarray = '';
            if (!$self->{_return_from_block}) {
                $wantarray = 'return';
            }

            if ( ! @{ $self->{arguments} } ) {
                return 'PerlOp.ret(PerlOp.context(' . Perlito5::Java::to_context($wantarray) . '))';
            }
            return 'PerlOp.ret(' . Perlito5::Java::to_runtime_context( $self->{arguments}, $level+1, $wantarray ) . ')';
        },
        'goto' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW_RETURN = 1;

            my $arg = $self->{arguments}->[0];
            if (   ( ref($arg) eq 'Perlito5::AST::Var'   && $arg->{sigil} eq '&' )
                || ( ref($arg) eq 'Perlito5::AST::Apply' && $arg->{code} eq 'prefix:<&>' ) )
            {
                # &subr &{"subr"} is a subroutine call
                return 'PerlOp.ret(' . $arg->emit_java($level) . ')';
            }

            return 'PerlOp.gotoOp('
                            . Perlito5::Java::to_context($wantarray) . ', '
                            . $arg->emit_java($level) . ', '
                            . 'List__'
                        . ')'
        },
        'caller' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'PlCORE.caller('
                            . Perlito5::Java::to_context($wantarray) . ', '
                            . Perlito5::Java::to_list($self->{arguments}, $level)
                        . ')'
        },

        'do' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # do BLOCK

                # this is disabled because we use "do-block" as a way to avoid the java error: "code too large"
                # if ($wantarray eq 'void') {
                #     return $arg->emit_java( $level, $wantarray );
                # }

                # rewrite to:   sub {...}->()
                my $ast = Perlito5::AST::Sub->new(
                    'block' => $arg,
                    'attributes' => [],
                    _do_block => 1,
                );
                return $ast->emit_java( $level + 1, $wantarray )
                    . '.apply_do_block('
                            . Perlito5::Java::to_context($wantarray) . ', '
                            . 'return_context, '
                            . 'List__'
                    . ')';
            }

            # do EXPR
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'slurp_file',
                          namespace => 'Perlito5::Grammar::Use',
                          arguments => $self->{arguments}
                        )
                    ],
                    _scope          => Perlito5::Grammar::Scope->new_base_scope(),
                    _hash_hints     => {},  # TODO
                    _scalar_hints   => 0,   # TODO
                );
            my $js = $ast->emit_java( $level, $wantarray );
            return $js;
        },

        'eval' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0] || Perlito5::AST::Var::SCALAR_ARG();
            my $eval;
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # eval BLOCK
                # rewrite to:   sub {...}->()

                # TODO - optimization - examine the block and set THROW conditionally
                $Perlito5::THROW = 1;

                my $ast = Perlito5::AST::Call->new(
                    'method' => 'postcircumfix:<( )>',
                    'invocant' => Perlito5::AST::Sub->new(
                        'block' => $arg,
                        'attributes' => [],
                        _eval_block => 1,
                    ),
                    'arguments' => [
                        Perlito5::AST::Var::LIST_ARG(),
                    ],
                );
                return $ast->emit_java( $level + 1, $wantarray );
            }

            # eval string

            if (!$Perlito5::JAVA_EVAL) {
                return q{PlCORE.die("This script has eval string disabled - the 'java_eval' switch is turned off")};
            }

            # See: Perlito5::JavaScript2::Runtime::perl5_to_js()
            # TODO - enumerate lexicals
            # TODO - move sentence inside a do-block
            # TODO - test return() from inside eval
            # TODO - test next() from inside eval

            my %vars;
            for my $var (@{ $self->{_scope}{block} }, @Perlito5::CAPTURES) {
                if ( $var->{_decl} && $var->{_decl} ne 'global' && $var->{_decl} ne 'our' ) {
                    $vars{ $var->{_real_sigil} || $var->{sigil} }{ $var->emit_java(0) } = $var;
                }
            }

            # %vars = {
            #   '@' => {
            #            'xx_101' => (ast)
            #          },
            #   '$' => {
            #            'x_100' => (ast)
            #          }
            # };

            # "$scope" contains the "my" declarations
            # scope only contains variables captured by the current subroutine,
            # and variables declared since the 'sub' started.

            my $scope = Perlito5::DumpToAST::dump_to_ast( $self->{_scope}, {}, "s" )->emit_java(0);
            # print STDERR "SCOPE [ $scope ]\n";

            $self->{_hash_hints} ||= {};    # FIXME
            my $hash_hints = Perlito5::DumpToAST::dump_to_ast( $self->{_hash_hints}, {}, "s" )->emit_java(0);

            my @out;
            {
                # set the new variable names inside the closure
                local %Perlito5::Java::Java_var_name;

                my %type = ( '$' => 'PlLvalue', '@' => 'PlArray', '%' => 'PlHash' );
                for my $sigil ( '$', '@', '%' ) {
                    my @str;
                    my @val;
                    for my $var ( keys %{ $vars{$sigil} } ) {
                        push @str, Perlito5::Java::escape_string( $vars{$sigil}{$var}->emit_java(0) );
                        push @val, $var;
                    }
                    push @out, 'new String[]{' . join(", ", @str) . '}';
                    push @out, 'new ' . $type{$sigil} . '[]{' . join(", ", @val) . '}';
                }
            }

            # new String[]{"x_100"};
            # new PlLvalue[]{x_100};
            # new String[]{"xx_101"};
            # new PlArray[]{xx_101};
            # new String[]{};
            # new PlHash[]{}

            return 'PlJavaCompiler.eval_perl_string('
                . $arg->emit_java( $level, $wantarray ) . '.toString(), '
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ', '
                . Perlito5::Java::escape_string($wantarray) . ', '
                . 'new PlInt(' . ( 0 + $self->{_scalar_hints} ) . 'L), '
                . $hash_hints . ', '
                . $scope . ', '
                . join( ', ', @out ) . ', '
                . Perlito5::Java::to_context($wantarray) . ', '
                . 'List__'
                . ')';
        },

        'length' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = shift @{$self->{arguments}};
            $arg->emit_java($level, 'scalar') . '.length()'
        },
        'substr' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my $meth = "substr";
            $meth = "lvalue_substr" if $autovivification_type eq 'lvalue';
            my $arg = shift @{$self->{arguments}};
                return $arg->emit_java($level, 'scalar') . '.' . $meth . '('
                    .   join(', ', map( $_->emit_java($level, 'scalar'), @{$self->{arguments}} ))
                    . ')'
        },
        'undef' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                my $arg = $self->{arguments}[0];
                if (  ref( $arg ) eq 'Perlito5::AST::Var' 
                   && $arg->{sigil} eq '&'
                   )
                {
                    my $name = $arg->{name};
                    my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                    return 'PlV.cset(' . Perlito5::Java::escape_string($namespace . '::' . $name) . ', PlCx.UNDEF)';
                }
                if (  ref( $arg ) eq 'Perlito5::AST::Apply' 
                   && $arg->{code} eq 'prefix:<*>'
                   )
                {
                    return $arg->emit_java($level, 'scalar') . '.setUndef()';
                }
                $self->{arguments} = [];
                return $arg->emit_java_set($self, $level, $wantarray);
            }
            return 'PlCx.UNDEF'
        },
        'defined' => sub { 
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}[0];
            my $invocant;
            if (  ref( $arg ) eq 'Perlito5::AST::Apply' 
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2   = $arg->{arguments}->[0];
                $invocant = 'PlV.code_lookup_by_name_no_autoload(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg2->emit_java($level) . ')';
            }
            elsif (  ref( $arg ) eq 'Perlito5::AST::Var' 
               && $arg->{sigil} eq '&'
               )
            {
                my $name = $arg->{name};
                my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                $invocant = 'PlV.cget_no_autoload(' . Perlito5::Java::escape_string($namespace . '::' . $name) . ')';
            }
            else {
                $invocant = $arg->emit_java($level, 'scalar');
            }
            # TODO - use this code for typed variables:
            #   'new PlBool(' . $invocant . ' != null)' 
            'new PlBool(!' . $invocant . '.is_undef())'
        },
        'shift' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_java( $level ) . '.shift()'
            }
            if ($Perlito5::Java::is_inside_subroutine) {
                return 'List__.shift()';                        # shift @_
            }
            return 'PlV.array_get("main::ARGV").shift()';  # shift @ARGV
        },
        'pop' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_java( $level ) . '.pop()'
            }
            if ($Perlito5::Java::is_inside_subroutine) {
                return 'List__.pop()';                          # pop @_
            }
            return 'PlV.array_get("main::ARGV").pop()';    # pop @ARGV
        },
        'unshift' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'
            return $v->emit_java( $level ) . '.unshift(' . Perlito5::Java::to_list_for_push(\@arguments, $level) . ')';
        },
        'push' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            if (@arguments == 1 && ref($arguments[0]) eq "Perlito5::AST::Var" && $arguments[0]->{sigil} eq '$') {
                return $v->emit_java( $level ) . '.push(' . $arguments[0]->emit_java( $level ) . ')';
            }

            return $v->emit_java( $level ) . '.push(' . Perlito5::Java::to_list_for_push(\@arguments, $level) . ')';
        },
        'splice' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $array  = shift(@arguments);
            my $offset = shift(@arguments);
            my $length = shift(@arguments);

            'PlCORE.splice(' . Perlito5::Java::to_context($wantarray) . ', '
                . $array->emit_java($level)
                . ($offset ? (', ' . $offset->emit_java($level)) : ())
                . ($length ? (', ' . $length->emit_java($level)) : ())
                . (@arguments ? (', ' . Perlito5::Java::to_list(\@arguments, $level)) : ())
            . ')';
        },

        'pos' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (!@arguments) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            'PerlOp.pos('
             .      $arguments[0]->emit_java($level, 'scalar')
             . ')';
        },
        'tie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;
            if (ref($v) eq "Perlito5::AST::Decl") {
                 # this argument can be a 'Decl' instead of 'Var'
                $v = $v->{var};
            }
            if ( ( $v->isa('Perlito5::AST::Var') && ( $v->{sigil} eq '%' || $v->{sigil} eq '@' ) ) 
              || ( $v->isa('Perlito5::AST::Apply') && ( $v->{code} eq 'prefix:<@>' || $v->{code} eq 'prefix:<%>' ) )
               )
            {
                return $v->emit_java($level) . '.tie(' . Perlito5::Java::to_list(\@arguments, $level) . ')';
            }
            return $v->emit_java( $level, 'scalar', 'lvalue' ) . '.tie(' . Perlito5::Java::to_list(\@arguments, $level) . ')';
        },
        'untie' => sub {
            my ( $self, $level, $wantarray ) = @_;
            my @arguments = @{ $self->{arguments} };
            my $v         = shift @arguments;
            return $v->emit_java($level) . '.untie()';
        },
        'tied' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;
            return $v->emit_java( $level ) . '.tied()';
        },
        'print' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun = Perlito5::Java::to_filehandle($self->{special_arg}, $level+1);
            }
            else {
                $fun  = 'PlV.STDOUT';
            }
            my $list = Perlito5::Java::to_list(\@in, $level);
            'PlCORE.print(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'say' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun = Perlito5::Java::to_filehandle($self->{special_arg}, $level+1);
            }
            else {
                $fun  = 'PlV.STDOUT';
            }
            my $list = Perlito5::Java::to_list(\@in, $level);
            'PlCORE.say(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'printf' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun = Perlito5::Java::to_filehandle($self->{special_arg}, $level+1);
            }
            else {
                $fun  = 'PlV.STDOUT';
            }
            my $list = 'new PlArray(PlCORE.sprintf(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list(\@in, $level) . '))';
            'PlCORE.print(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'select' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments  = @{$self->{arguments}};
            if ( @arguments == 1 ) {
                return 'PlCORE.select(' . Perlito5::Java::to_filehandle($arguments[0], $level+1) . ')';
            }
            'PlCORE.select(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'mkdir' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (@arguments < 1) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            if (@arguments < 2) {
                push @arguments,
                    Perlito5::AST::Int->new(
                        int => 0777
                    );
            }
            'PlCORE.mkdir(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'rmdir' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (@arguments < 1) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            'PlCORE.rmdir(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'chdir' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (@arguments < 1) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            'PlCORE.chdir(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'unlink' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (@arguments < 1) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            'PlCORE.unlink(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'getc' => sub {
            my ($self, $level, $wantarray) = @_;
            # getc FILEHANDLE
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            if ( $fun ) {
                $fun = Perlito5::Java::to_filehandle($fun, $level+1);
            }
            else {
                $fun  = 'PlV.STDIN';
            }
            'PlCORE.getc('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      $fun . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        },
        'readline' => sub {
            my ($self, $level, $wantarray) = @_;
            # readline FILEHANDLE
            # TODO - special cases; see 'readline' and '<>' in "perldoc perlop"
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in)
                || bless({
                       'arguments' => [],
                       'bareword' => 1,
                       'code' => 'ARGV',
                       'namespace' => 'main',
                   }, 'Perlito5::AST::Apply');
            my $list = Perlito5::Java::to_list(\@in, $level);
            'PlCORE.readline(' . Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      $list
             . ')';
        },
        'stat' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            @in = Perlito5::AST::Var::SCALAR_ARG() if !@in;
            'PlCORE.stat(' . Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_list(\@in, $level)
             . ')';
        },
        'map' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Java::to_list(\@in, $level);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.map(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . 'List__, '
                . Perlito5::Java::to_context($wantarray) . ')';
        },
        'grep' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Java::to_list(\@in, $level);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.grep(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . 'List__, '
                . Perlito5::Java::to_context($wantarray) . ')';
        },
        'bless' => sub {
            my ($self, $level, $wantarray) = @_;
            my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );
            my @in  = @$items;
            my $ref = shift @in;
            my $class = shift @in;
            if ($class) {
                $class = Perlito5::Java::to_native_str($class);
            }
            else {
                $class = Perlito5::Java::escape_string($Perlito5::PKG_NAME);
            }

            return $ref->emit_java( $level, "scalar" )
                . '.bless(' . $class . ')';
        },
        'sort' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            my $list;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                if (ref($in[0]) eq 'Perlito5::AST::Block') {
                    # the sort function is optional
                    $fun  = shift @in;
                }
            }

            if (ref($fun) eq 'Perlito5::AST::Block') {
                # the sort function is optional
                $fun = $fun->{stmts};
            }
            else {
                die "TODO: sort without block not implemented yet";
            }
            $list = Perlito5::Java::to_list(\@in, $level);

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.sort(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . 'List__, '
                . Perlito5::Java::to_context($wantarray)
            . ')';
        },
        'ref' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            return $arg->emit_java($level, 'scalar') . '.ref()';
        },
        'exists' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    # $v->{sigil} = '%';
                    return $v->emit_java($level, $wantarray) . '.hexists('
                        .   Perlito5::Java::to_native_str($arg->autoquote($arg->{index_exp}), $level)
                        . ')';
                }

                if (  $v->isa('Perlito5::AST::Apply')
                   && $v->{code} eq 'prefix:<$>'
                   )
                {
                    # $$a{0} ==> $a->{0}
                    $arg = Perlito5::AST::Call->new(
                        'method' => 'postcircumfix:<{ }>',
                        'invocant' => $v->{arguments}[0],
                        'arguments' => $arg->{index_exp},
                    );
                }
                else {
                    return $v->emit_java($level, $wantarray, 'hash') . '.hexists('
                        .   Perlito5::Java::to_native_str($arg->autoquote($arg->{index_exp}), $level)
                        . ')';
                }
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    return $v->emit_java($level, 'array') . '.aexists(' . $arg->{index_exp}->emit_java($level) . ')';
                }

                if (  $v->isa('Perlito5::AST::Apply')
                   && $v->{code} eq 'prefix:<$>'
                   )
                {
                    # $$a[0] ==> $a->[0]
                    $arg = Perlito5::AST::Call->new(
                        'method' => 'postcircumfix:<[ ]>',
                        'invocant' => $v->{arguments}[0],
                        'arguments' => $arg->{index_exp},
                    );
                }
                else {
                    return $v->emit_java($level, $wantarray, 'array') . '.aexists(' . $arg->{index_exp}->emit_java($level) . ')';
                }
            }

            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.hexists('
                        .   Perlito5::Java::to_native_str(Perlito5::AST::Lookup->autoquote($arg->{arguments}), $level)
                        . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.aexists(' . $arg->{arguments}->emit_java($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                # TODO exist() + 'my sub'
                my $name = $arg->{name};
                my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                return 'new PlBool(PlV.cget_no_autoload(' . Perlito5::Java::escape_string($namespace . '::' . $name) . ').is_coderef())';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2 = $arg->{arguments}->[0];
                return 'new PlBool('
                    .   'PlV.code_lookup_by_name_no_autoload(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg2->emit_java($level) . ')'
                    . '.is_coderef())';
            }
        },

        'prototype' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            return 'PerlOp.prototype(' . $arg->emit_java($level) . ', ' . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ')';
        },
        'split' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            if (@arguments < 1) {
                push @arguments, Perlito5::AST::Buf->new(buf => " ");
            }
            if (@arguments < 2) {
                push @arguments, Perlito5::AST::Var::SCALAR_ARG();
            }
            if (@arguments < 3) {
                push @arguments, Perlito5::AST::Int->new(int => 0);
            }
            my @js;
            my $arg = $arguments[0];
            if ( $arg
              && $arg->isa('Perlito5::AST::Apply')
              && $arg->{code} eq 'p5:m'
            ) {
                # first argument of split() is a regex
                my $flags = $arg->{arguments}->[1]->{buf};
                $flags .= "m" if $flags !~ /m/;     # split defaults to multiline
                push @js, emit_qr_java( $arg->{arguments}->[0], $flags );
                shift @arguments;
            }
            return 'PlCORE.split('
                . join( ', ',
                    Perlito5::Java::to_context($wantarray),
                    @js,
                    map( $_->emit_java($level), @arguments )
                  )
            . ')';
        },
    );

    for my $op (qw/ binmode close closedir open opendir readdir seek seekdir read sysread /) {
        $emit_js{$op} = sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'PlCORE.' . $op . '('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        };
    }

    for my $op (qw/ chomp chop /) {
        $emit_js{$op} = sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.' . $op . '('
            .   Perlito5::Java::to_context($wantarray) . ', '
            .   Perlito5::Java::to_param_list($self->{arguments}, $level+1)
            . ')';
        };
    }
    for my $op (qw/ hex oct fc values keys each /) {
        $emit_js{$op} = sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.' . $op . '('
            .   Perlito5::Java::to_context($wantarray) . ', '
            .   $self->{arguments}[0]->emit_java($level + 1)
            . ')';
        };
    }
    for my $op (qw/
        sleep exit warn die system qx pack unpack sprintf crypt join reverse
        gmtime localtime time times /
    ) {
        $emit_js{$op} = sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.' . $op . '('
            .   Perlito5::Java::to_context($wantarray) . ', '
            .   Perlito5::Java::to_list($self->{arguments}, $level + 1)
            . ')';
        };
    }

    sub emit_java_op_table { return \%emit_js }

    sub emit_java {
        my ($self, $level, $wantarray, $autovivification_type) = @_;

        my $code = $self->{code};

        if (ref $code ne '') {
            my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );

            if ( ref($code) eq 'Perlito5::AST::Apply' && $code->code eq "prefix:<&>") {
                # &$c()
                my $invocant = $code->{arguments}->[0]->emit_java($level);
                if ( !$code->{_strict_refs} ) {
                    $invocant = 'PlV.code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $invocant . ')';
                }
                return $invocant . '.apply('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . Perlito5::Java::to_param_list($items, $level+1)
                  . ')';
            }

            return $self->{code}->emit_java( $level ) . '.apply('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . Perlito5::Java::to_param_list($items, $level+1)
                  . ')';
        }

        if (!exists($emit_js{$code})) {
            my $apply = $self->op_assign();
            if ($apply) {
                return $apply->emit_java( $level );
            }
            my $apply = $self->op_auto();
            if ($apply) {
                return $apply->emit_java( $level );
            }
        }

        return ''
            if $code eq 'package';
        return $emit_js{$code}->($self, $level, $wantarray, $autovivification_type)
            if exists $emit_js{$code} && ($self->{namespace} eq '' || $self->{namespace} eq 'CORE');

        if (exists $Perlito5::Java::op_prefix_js_str{$code}) {
            return $Perlito5::Java::op_prefix_js_str{$code} . '(' 
                . Perlito5::Java::to_str($self->{arguments}[0])
                . ')'
        }

        if ($self->{namespace}) {
            if (  $self->{namespace} eq 'Java' 
               && $code eq 'inline'
               ) 
            {
                my @args = @{ $self->{arguments} };
                if ( @args != 1 ) {
                    die "Java::inline needs a single argument";
                }
                if ( $args[0]->isa('Perlito5::AST::Apply') && $args[0]{code} eq 'list:<.>') {
                    @args = @{ $args[0]{arguments} };
                    if ( @args != 1 ) {
                        die "Java::inline needs a string constant, got:", Perlito5::Dumper::Dumper(\@args);
                    }
                }
                if ( $args[0]->isa('Perlito5::AST::Buf') ) {
                    # Java::inline('int x = 123')
                    return $args[0]{buf};
                }
                else {
                    die "Java::inline needs a string constant, got:", Perlito5::Dumper::Dumper(\@args);
                }
            }
            if ($self->{namespace} eq 'Perlito5') {
                if ($code eq 'eval_ast') {
                    $self->{namespace} = 'Perlito5::Java::Runtime';
                }
            }
            $code = $self->{namespace} . '::' . $code;
        }
        else {
            $code = $Perlito5::PKG_NAME . '::' . $code;
        }

        my $sig;
        my $may_need_autoload;
        {
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            my $effective_name = $namespace . "::" . $self->{code};
            if ( exists $Perlito5::PROTO->{$effective_name} ) {
                $sig = $Perlito5::PROTO->{$effective_name};
            }
            elsif ( (!$self->{namespace} || $namespace eq 'CORE')
                  && exists $Perlito5::CORE_PROTO->{"CORE::$name"}
                  )
            {
                $effective_name = "CORE::$name";
                $sig = $Perlito5::CORE_PROTO->{$effective_name};
            }
            else {
                # this subroutine was never declared
                if ($self->{bareword}) {
                    # TODO: allow barewords where a glob is expected: open FILE, ...

                    # bareword doesn't call AUTOLOAD
                    return Perlito5::AST::Buf->new(
                        buf => ($self->{namespace} ? $self->{namespace} . '::' : "") . $name,
                    )->emit_java( $level + 1, 'scalar' );
                }
                $may_need_autoload = 1;
            }
            # is there a sig override
            $sig = $self->{proto}
                if (exists $self->{proto});
        }

        $sig = ""
            if $self->{ignore_proto};

        if ($sig) {
            # warn "sig $effective_name $sig\n";
            my @out = ();
            my @in  = @{$self->{arguments} || []};

            # TODO - generate the right prototype

            my $optional = 0;
            while (length $sig) {
                my $c = substr($sig, 0, 1);
                if ($c eq ';') {
                    $optional = 1;
                }
                elsif ($c eq '$' || $c eq '_') {
                    push @out, shift(@in)->emit_java( $level + 1, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '@') {
                    push @out, Perlito5::Java::to_list(\@in, $level + 1)
                        if @in || !$optional;
                    @in = ();
                }
                elsif ($c eq '&') {
                    push @out, shift(@in)->emit_java( $level + 1, 'scalar' );
                }
                elsif ($c eq '*') {
                    if (@in || !$optional) {
                        my $arg = shift @in;
                        if ($arg->{bareword}) {
                            push @out, Perlito5::AST::Buf->new(buf => $arg->{code})->emit_java( $level + 1, 'scalar' );
                        }
                        else {
                            push @out, $arg->emit_java( $level + 1, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_java( $level + 1, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 6) eq '\\[$@%]') {
                        $sig = substr($sig, 5);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                }
                $sig = substr($sig, 1);
            }

            # create a PlStringConstant
            $code = Perlito5::AST::Buf->new( buf => $code )->emit_java($level, 'scalar');
            return $code . '.apply(' 
                        . Perlito5::Java::to_context($wantarray)
                        . ', PlArray.construct_list_of_aliases(' . join(', ', @out) . ')'
                . ')';
        }

        my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );

        # create a PlStringConstant
        $code = Perlito5::AST::Buf->new( buf => $code )->emit_java($level, 'scalar');
        return $code . '.apply('
                . Perlito5::Java::to_context($wantarray) . ', '
                . Perlito5::Java::to_param_list($items, $level+1)
              . ')';

    }

    sub emit_java_get_decl {
        my $self      = shift;
        my $code = $self->{code};
        if ($code ne 'do' && $code ne 'eval') {
            return ( map  +( $_->emit_java_get_decl ), 
                          @{ $self->{arguments} }
                   )
                if $self->{arguments};
        }
        return ()
    }
    sub emit_java_has_regex {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'p5:m' || $code eq 'p5:s' || $code eq 'infix:<=~>' || $code eq 'infix:<!~>') {
            return 1;
        }
        return ()
    }
}

1;


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

        my $s = 'new PlRegex(' . Perlito5::Java::to_str( $regex ) . ', ' . $flag_string . ')';
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

        if ($regex->isa('Perlito5::AST::Var')) {
            # $x =~ $regex
            $regex = { code => 'p5:m', arguments => [ $regex, '' ] };
        }

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
                    . $var->emit_java($level) . ', '
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

        if ( $code eq 'my' || $code eq 'state' || $code eq 'local' || $code eq 'circumfix:<( )>' ) {
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
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
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
            Perlito5::Java::escape_string($Perlito5::PKG_NAME);
        },
        '__SUB__' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::AST::Sub::SUB_REF // 'this'
        },
        'wantarray' => sub {
            my ($self, $level, $wantarray) = @_;
            '(want == PlCx.VOID ? PlCx.UNDEF : new PlInt(want-1))';
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
        'ucfirst' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.ucfirst()'
        },
        'lcfirst' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.lcfirst()'
        },
        'quotemeta' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.quotemeta()'
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
              ? $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long()'
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
            qw/ abs sqrt cos sin exp log /
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

        'infix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
            return $self->{arguments}->[0]->emit_java($level, 'scalar') . '.mod('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<>>>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() >>> '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<<<>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() << '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<^>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() ^ '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() & '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<|>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() | '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.add('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.sub('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<*>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.mul('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:</>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.div('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<==>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_eq('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<!=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_ne('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<>>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_gt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<>=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_ge('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_lt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_le('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<eq>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_eq('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<ne>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_ne('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<gt>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_gt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<ge>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_ge('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<lt>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_lt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<le>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_le('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
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
            if ($wantarray eq 'void') {
                return
                  '(' . Perlito5::Java::to_boolean($self->{arguments}->[0], $level) . ' ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PlCx.UNDEF)';
            }
            # and1(x) ? y : and3()
            '(PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PerlOp.and3())'
        },
        'infix:<and>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  '(' . Perlito5::Java::to_boolean($self->{arguments}->[0], $level) . ' ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PlCx.UNDEF)';
            }
            # and1(x) ? y : and3()
            '(PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ' : PerlOp.and3())'
        },
        'infix:<||>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  '(' . Perlito5::Java::to_boolean($self->{arguments}->[0], $level) . ' ? '
                . ' PlCx.UNDEF : ' . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')';
            }
            # or1(x) ? or2() : y
            '(PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
                . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')'
        },
        'infix:<or>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  '(' . Perlito5::Java::to_boolean($self->{arguments}->[0], $level) . ' ? '
                . ' PlCx.UNDEF : ' . $self->{arguments}->[1]->emit_java($level, $wantarray) . ')';
            }
            # or1(x) ? or2() : y
            '(PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
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
        'infix:<cmp>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_cmp('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_cmp('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<**>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.pow('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'atan2' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.atan2('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
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
                return 'p5pkg["Perlito5"]["test_perl_version"]([' 
                        . Perlito5::Java::to_str( $self->{arguments}[0] )
                    . '], ' . Perlito5::Java::to_context($wantarray) . ')';
            }
            # require FILE
            'PlCORE.require('
                . Perlito5::Java::to_context($wantarray) . ', '
                . Perlito5::Java::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 'true' : 'false') 
            . ')';
        },
        'select' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5pkg["CORE"]["select"]([' 
                . ( $self->{arguments}[0]{bareword}
                  ? Perlito5::Java::to_str( $self->{arguments}[0] )
                  : $self->{arguments}[0]->emit_java( $level, 'scalar' ) )
            . '])';
        },
        'prefix:<$>' => sub {
            my ($self, $level, $wantarray, $autovivification_type) = @_;
            my $arg  = $self->{arguments}->[0];
            if ($autovivification_type eq 'lvalue') {
                return $arg->emit_java( $level, 'scalar', 'lvalue' ) . '.scalar_deref_lvalue('
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME )
                    . ')';
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
            else {
                $s = Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref()';
            }
            return $wantarray eq 'scalar'
                ? "$s.scalar()"
                : $s;
        },
        'prefix:<$#>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return  Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref().end_of_array_index()';
        },
        'prefix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return Perlito5::Java::emit_java_autovivify( $arg, $level, 'hash' ) . '.hash_deref()';
        },
        'prefix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            'PlV.code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_java($level) . ')'
                . '.apply('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . 'List__'
                . ')'
        },
        'prefix:<*>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return Perlito5::Java::to_filehandle($arg, $level+1);
        },
        'circumfix:<[ ]>' => sub {
            my ($self, $level, $wantarray) = @_;
            'new PlArrayRef(new PlArray(' . Perlito5::Java::to_list( $self->{arguments}, $level ) . '))';
        },
        'circumfix:<{ }>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new PlHashRef(new PlHash(' . Perlito5::Java::to_list( $self->{arguments}, $level ) . ')))';
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
                    return 'p5_list_of_refs(' . Perlito5::Java::to_list( $arg->{arguments}, $level ) . ')';
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
                    return '(new PlHashRef(' . $arg->emit_java($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new PlGlobRef(' . $arg->emit_java($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                    return 'PlV.cget(' . Perlito5::Java::escape_string($namespace . '::' . $arg->{name} ) . ')'
                }
            }
            return '(new PlLvalueRef(' . $arg->emit_java($level) . '))';
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
            'PerlOp.string_replicate('
                           . Perlito5::Java::to_str($self->{arguments}->[0], $level) . ','
                           . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
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
                    return $v->emit_java($level) . '.delete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'hash') . '.delete(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->{_real_sigil} eq '@'
                   )
                {
                    $v = Perlito5::AST::Var->new(%$v, sigil => '@');
                    return $v->emit_java($level) . '.delete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->{index_exp}->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'array') . '.delete(' . $arg->{index_exp}->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.delete(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_java($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.delete(' . $arg->{arguments}->emit_java($level) . ')';
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

        'scalar' => sub {
            my ($self, $level, $wantarray) = @_;
            if (@{$self->{arguments}} > 1) {
                return 'PerlOp.context(' . join( ', ', Perlito5::Java::to_context('scalar'), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
            }
            Perlito5::Java::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::Java::to_boolean( $self->{arguments}->[0], $level ) . ' ? ' . ( $self->{arguments}->[1] )->emit_java( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_java( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of my($x,$y)
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'state' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of state($x,$y)
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'our' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of our($x,$y)
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'local' => sub {
            my ($self, $level, $wantarray) = @_;
            # 'local ($x, $y[10])'
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'circumfix:<( )>' => sub {
            my ($self, $level, $wantarray) = @_;
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<=>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];
            return $parameters->emit_java_set($arguments, $level+1, $wantarray);
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
            $Perlito5::THROW_RETURN = 1;
            if ( ! @{ $self->{arguments} } ) {
                return 'PerlOp.ret(PerlOp.context(want))';
            }
            'PerlOp.ret(' . Perlito5::Java::to_runtime_context( $self->{arguments}, $level+1 ) . ')';
        },
        'goto' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW_RETURN = 1;

            my $arg = $self->{arguments}->[0];
            if (  ref($arg) eq 'Perlito5::AST::Var'
               && $arg->{sigil} eq '&'
               )
            {
                # &subr is a subroutine call
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
            return 'PerlOp.caller('
                            . Perlito5::Java::to_context($wantarray) . ', '
                            . Perlito5::Java::to_list($self->{arguments}, $level)
                        . ')'
        },

        'do' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # do BLOCK
                # rewrite to:   sub {...}->()
                my $ast = Perlito5::AST::Sub->new(
                    'block' => $arg,
                    'attributes' => [],
                    _do_block => 1,
                );
                return $ast->emit_java( $level + 1, $wantarray )
                    . '.apply(' . Perlito5::Java::to_context($wantarray) . ', List__)';
            }

            # do EXPR
            my $tmp_strict = $Perlito5::STRICT;
            $Perlito5::STRICT = 0;
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'do_file',
                          namespace => 'Perlito5::Grammar::Use',
                          arguments => $self->{arguments}
                        )
                    ],
                    _scope => Perlito5::Grammar::Scope->new_base_scope(),
                );
            my $js = $ast->emit_java( $level, $wantarray );
            $Perlito5::STRICT = $tmp_strict;
            return $js;
        },

        'eval' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
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
                if ( $var->{_decl} && $var->{_decl} ne 'global' ) {
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

    print STDERR "eval scope ", Perlito5::Dumper::ast_dumper( $scope );
    print STDERR "eval vars ", Perlito5::Dumper::ast_dumper(\%vars);

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
                . ( 0 + $Perlito5::STRICT ) . ', '
                . $scope . ', '
                . join( ', ', @out ) . ', '
                . 'List__'
                . ')';
        },

        'length' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = shift @{$self->{arguments}};
                return Perlito5::Java::to_str($arg) 
                    . '.length('
                    . ')'
        },
        'substr' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = shift @{$self->{arguments}};
                return Perlito5::Java::to_str($arg) 
                    . '.substr('
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
                    return '(delete p5pkg[' . Perlito5::Java::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::Java::escape_string($arg->{name} ) . '])';
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
                $invocant = 'p5pkg[' . Perlito5::Java::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::Java::escape_string($arg->{name} ) . ']';
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
            return $v->emit_java( $level ) . '.unshift(' . Perlito5::Java::to_list(\@arguments, $level) . ')';
        },
        'push' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            if (@arguments == 1 && ref($arguments[0]) eq "Perlito5::AST::Var" && $arguments[0]->{sigil} eq '$') {
                return $v->emit_java( $level ) . '.push(' . $arguments[0]->emit_java( $level ) . ')';
            }

            return $v->emit_java( $level ) . '.push(' . Perlito5::Java::to_list(\@arguments, $level) . ')';
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
        'time' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.time(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'sleep' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.sleep(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'ref' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.ref(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'exit' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.exit(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'warn' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.warn(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'die' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.die(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'system' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.system(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'qx' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.qx(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'tie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;
            if (ref($v) eq "Perlito5::AST::Decl") {
                 # this argument can be a 'Decl' instead of 'Var'
                $v = $v->{var};
            }
            my $meth;
            if ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '%' ) {
                $meth = 'hash';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '@' ) {
                $meth = 'array';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '$' ) {
                $meth = 'scalar';
            }
            else {
                die "tie '", ref($v), "' not implemented";
            }
            my $tie = 'PerlOp.tie_' . $meth . '(' . $v->emit_java( $level ) . ', ' . Perlito5::Java::to_list(\@arguments, $level) . ')';
            if ($v->{_decl} eq 'global') {
                return $v->emit_java_global_set_alias($tie, $level);
            }
            else {
                return $v->emit_java( $level ) . ' = ' . $tie;
            }
        },
        'untie' => sub {
            my ( $self, $level, $wantarray ) = @_;
            my @arguments = @{ $self->{arguments} };
            my $v         = shift @arguments;
            my $tie       = $v->emit_java($level) . '.untie()';
            if ( $v->{_decl} eq 'global' ) {
                return $v->emit_java_global_set_alias( $tie, $level );
            }
            else {
                return $v->emit_java($level) . ' = ' . $tie;
            }
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
                $fun  = 'PlCx.STDOUT';
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
                $fun  = 'PlCx.STDOUT';
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
                $fun  = 'PlCx.STDOUT';
            }
            my $list = 'new PlArray(PlCORE.sprintf(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list(\@in, $level) . '))';
            'PlCORE.print(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'hex' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.hex(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'oct' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.oct(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'sprintf' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.sprintf(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'crypt' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.crypt(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'join' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.join(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'reverse' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.reverse(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'fc' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.fc(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'pack' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.pack(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'unpack' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.unpack(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}, $level) . ')';
        },
        'values' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.values(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'keys' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.keys(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'each' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.each(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
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
        'close' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'PlCORE.close('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        },
        'open' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            $Perlito5::STRICT = 0;  # allow FILE bareword
            'PlCORE.open('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        },
        'chomp' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.chomp(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'chop' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.chop(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
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
                $fun  = 'PlCx.STDIN';
            }
            'PlCORE.getc('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      $fun . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        },
        'read' => sub {
            my ($self, $level, $wantarray) = @_;
            # read FILEHANDLE,SCALAR,LENGTH,OFFSET
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'PlCORE.read('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
             . ')';
        },
        'sysread' => sub {
            my ($self, $level, $wantarray) = @_;
            # sysread FILEHANDLE,SCALAR,LENGTH,OFFSET
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'PlCORE.sysread('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
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
        'seek' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'PlCORE.seek('
             .      Perlito5::Java::to_context($wantarray) . ', '
             .      Perlito5::Java::to_filehandle($fun, $level+1) . ', '
             .      Perlito5::Java::to_param_list(\@in, $level+1)  
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
        'infix:<//>' => sub { 
            my ($self, $level, $wantarray) = @_;
            # defined_or1(x) ? defined_or2() : y
            '(PerlOp.defined_or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.defined_or2() : '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
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
                    return $v->emit_java($level, $wantarray) . '.exists(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'hash') . '.exists(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    return $v->emit_java($level, 'array') . '.exists(' . $arg->{index_exp}->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'array') . '.exists(' . $arg->{index_exp}->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.exists(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_java($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.exists(' . $arg->{arguments}->emit_java($level) . ')';
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

    sub emit_java {
        my ($self, $level, $wantarray, $autovivification_type) = @_;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_java( $level );
        }
        my $apply = $self->op_auto();
        if ($apply) {
            return $apply->emit_java( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );

            if ( ref($code) eq 'Perlito5::AST::Apply' && $code->code eq "prefix:<&>") {
                # &$c()

                my $arg   = $code->{arguments}->[0];
                my $invocant = 'PlV.code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_java($level) . ')';

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

        return ''
            if $code eq 'package';
        return $emit_js{$code}->($self, $level, $wantarray, $autovivification_type)
            if exists $emit_js{$code} && ($self->{namespace} eq '' || $self->{namespace} eq 'GLOBAL');

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
            $code = 'PlV.cget(' . Perlito5::Java::escape_string($self->{namespace} . '::' . $code ) . ')'
        }
        else {
            $code = 'PlV.cget(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME . '::' . $code ) . ')'
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
                    # if ( $Perlito5::STRICT ) {
                    #     die 'Bareword ' . Perlito5::Java::escape_string($name ) . ' not allowed while "strict subs" in use';
                    # }

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

            return $code . '.apply('
                        . Perlito5::Java::to_context($wantarray)
                        . ', PlArray.construct_list_of_aliases(' . join(', ', @out) . ')'
                        # . Perlito5::Java::to_param_list(\@in, $level+1)  # TODO
                . ')';
        }

        my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );

        # TODO - autoload
        # if ( $may_need_autoload ) {
        #     # p5cget(namespace, name).apply(list, want)
        #     my $name = $self->{code};
        #     my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
        #     return 'p5cget('
        #             . Perlito5::Java::escape_string($namespace) . ', '
        #             . Perlito5::Java::escape_string($name) . ').apply('
        #             . $arg_code . ', '
        #             . Perlito5::Java::to_context($wantarray)
        #          . ')';
        # }

        $code . '.apply('
                . Perlito5::Java::to_context($wantarray) . ', '
                . Perlito5::Java::to_param_list($items, $level+1)
              . ')';

    }

    sub emit_java_get_decl {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'my' || $code eq 'state' || $code eq 'local') {
            # $self->{code} = 'circumfix:<( )>';
            return ( map {     ref($_) eq 'Perlito5::AST::Var'
                             ? Perlito5::AST::Decl->new(
                                 decl => $code,
                                 type => '',     # TODO - add type
                                 var  => $_,
                               )
                             : ()
                         }
                         @{ $self->{arguments} }
                   );
        }
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


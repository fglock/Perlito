package Perlito5::AST::Apply;
{
    use strict;

    sub _emit_assignment_javascript2 {
        my ($parameters, $arguments, $level, $wantarray) = @_;
        if (   $parameters->isa( 'Perlito5::AST::Apply' )
            &&  ( $parameters->code eq 'my' || $parameters->code eq 'local' || $parameters->code eq 'circumfix:<( )>' )
        )
        {
            # my ($x, $y) = ...
            # local ($x, $y) = ...
            # ($x, $y) = ...

            # Note - disabled optimization, because:
            #   "ARG1 && ARG2" in void context, ARG2 is called in void context
            #   but it needs wrapping
            # if ( $wantarray eq 'void' ) {
            #     my $tmp  = Perlito5::JavaScript2::get_label();
            #     return join( ";\n" . Perlito5::JavaScript2::tab($level),
            #         'var ' . $tmp  . ' = ' . Perlito5::JavaScript2::to_list([$arguments], $level+1),
            #         ( map $_->emit_javascript2_set_list($level, $tmp),
            #             @{ $parameters->arguments }
            #         ),
            #     );
            # }

            my $tmp  = Perlito5::JavaScript2::get_label();
            my $tmp2 = Perlito5::JavaScript2::get_label();
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray, 
                'var ' . $tmp  . ' = ' . Perlito5::JavaScript2::to_list([$arguments], $level+1) . ";",
                'var ' . $tmp2 . ' = ' . $tmp . ".slice(0);",
                ( map $_->emit_javascript2_set_list($level+1, $tmp) . ";",
                    @{ $parameters->arguments }
                ),
                'return ' . $tmp2,
            );
        }
        return $parameters->emit_javascript2_set($arguments, $level+1, $wantarray);
    }

    sub emit_regex_javascript2 {
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
        if ($code eq 'p5:s') {
            my $replace = $regex_args->[1];
            my $modifier = $regex_args->[2]->{buf};
            my $fun;
            if (ref($replace) eq 'Perlito5::AST::Block') {
                $replace = Perlito5::AST::Sub->new(
                            block => $replace,
                        );
                $fun = $replace->emit_javascript2($level+2, $wantarray);
                $modifier =~ s/e//g;
            }
            else {
                $fun = Perlito5::JavaScript2::emit_function_javascript2($level+2, $wantarray, $replace);
            }
            $str = Perlito5::JavaScript2::emit_wrap_javascript2($level+1, $wantarray, 
                "var tmp = p5s("
                    . $var->emit_javascript2() . ', '
                    . $regex_args->[0]->emit_javascript2() . ', '
                    . $fun . ', '
                    . Perlito5::JavaScript2::escape_string($modifier) . ', '
                    . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 )
                  . ");",
                $var->emit_javascript2() . " = tmp[0];",
                "return tmp[1];",
            );
        }
        elsif ($code eq 'p5:m') {
            $str = 'p5m('
                    . $var->emit_javascript2() . ', '
                    . $regex_args->[0]->emit_javascript2() . ', '
                    . Perlito5::JavaScript2::escape_string($regex_args->[1]->{buf}) . ', '
                    . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 )
                  . ")";
        }
        elsif ($code eq 'p5:tr') {
            $str = Perlito5::JavaScript2::emit_wrap_javascript2($level+1, $wantarray, 
                "var tmp = p5tr("
                    . $var->emit_javascript2() . ', '
                    . $regex_args->[0]->emit_javascript2() . ', '
                    . $regex_args->[1]->emit_javascript2() . ', '
                    . Perlito5::JavaScript2::escape_string($regex_args->[2]->{buf}) . ', '
                    . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 )
                  . ");",
                $var->emit_javascript2() . " = tmp[0];",
                "return tmp[1];",
            );
        }
        else {
            die "Error: regex emitter - unknown operator $code";
        }

        if ($op eq '=~') {
            return $str;
        }
        if ($op eq '!~') {
            return '!(' . $str . ')'
        }
        die "Error: regex emitter";
    }

    sub emit_javascript2_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $code = $self->{code};
        if ($code eq 'prefix:<$>') {
            return 'p5scalar_deref_set(' 
                . Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . ', '
                . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)  . ', '
                . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        if ($code eq 'prefix:<*>') {
            return 'p5typeglob_deref_set(' 
                . Perlito5::JavaScript2::to_scalar($self->{arguments}, $level+1) . ', '
                . Perlito5::JavaScript2::to_scalar([$arguments], $level+1)       . ', '
                . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        $open . $self->emit_javascript2( $level+1 ) . ' = ' . $arguments->emit_javascript2( $level+1 ) . $close;
    }

    my %emit_js = (
        'infix:<=~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'infix:<!~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '!~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'p5:s' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:m' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', $self->{arguments}->[2], $self, $level, $wantarray );
        },
        'p5:tr' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_javascript2( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:qr' => sub {
            my ($self, $level, $wantarray) = @_;
            # p5qr( $str, $modifier );
            'p5qr(' . Perlito5::JavaScript2::to_str( $self->{arguments}[0] ) . ', '
                    . Perlito5::JavaScript2::to_str( $self->{arguments}[1] ) . ')';
        },
        '__PACKAGE__' => sub {
            my $self = $_[0];
            Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME);
        },
        '__SUB__' => sub {
            my $self = $_[0];
            $Perlito5::AST::Sub::SUB_REF // '__SUB__'
        },
        'wantarray' => sub {
            my $self = $_[0];
            'p5want';
        },
        'package' => sub {
            my $self = $_[0];
            'p5make_package(' . Perlito5::JavaScript2::escape_string($self->{namespace} ) . ')';
        },
        'bless' => sub {
            my ($self, $level, $wantarray) = @_;
            my $class;
            if ($self->{arguments}[1]) {
                $class = Perlito5::JavaScript2::to_str( $self->{arguments}[1] );
            }
            else { 
                $class = Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME);
            }
            'CORE.bless(p5list_to_a([' . $self->{arguments}[0]->emit_javascript2($level, 'scalar') . ', ' . $class . ']))';
        },
        'infix:<~~>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg0 = $self->{arguments}->[0];
            my $arg1 = $self->{arguments}->[1];
            # TODO - test argument type
            #   See: http://perldoc.perl.org/perlop.html#Smartmatch-Operator
            # if (Perlito5::JavaScript2::is_num($arg1)) {
            #     # ==
            # }
            'p5smrt_scalar(' . $arg0->emit_javascript2($level, 'scalar') . ', '
                             . $arg1->emit_javascript2($level, 'scalar') . ')'
        },
        'infix:<&&>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5and('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<and>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5and('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<||>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5or('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<or>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5or('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<xor>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5xor('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
        },
        'infix:<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'p5list_to_a([' .
              Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_javascript2($level)  . ', '
            . $self->{arguments}[1]->emit_javascript2($level)
            . '])';
        },
        'infix:<cmp>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::JavaScript2::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<=>>' => sub {
            my $self = $_[0];
            'p5cmp(' . join( ', ', map( Perlito5::JavaScript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<**>' => sub {
            my $self = $_[0];
            'Math.pow(' . join( ', ', map( Perlito5::JavaScript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<<<>' => sub {
            my $self = $_[0];
            'p5shift_left(' . join( ', ', map( Perlito5::JavaScript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<%>' => sub {
            my $self = $_[0];
            'p5modulo(' . join( ', ', map( Perlito5::JavaScript2::to_num($_), @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            '!( ' . Perlito5::JavaScript2::to_bool( $self->{arguments}->[0], $level ) . ')';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $arg = pop(@{$self->{arguments}});
            if (!$arg) {
                return 'true';
            }
            '!( ' . Perlito5::JavaScript2::to_bool( $arg, $level ) . ')';
        },
        'prefix:<~>' => sub {
            my $self = $_[0];
            'p5complement( ' . Perlito5::JavaScript2::to_num( $self->{arguments}->[0] ) . ')';
        },
        'prefix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5negative( ' . $self->{arguments}->[0]->emit_javascript2( $level, 'scalar' ) . ')';
        },
        'prefix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(' . $self->{arguments}->[0]->emit_javascript2( $level, $wantarray ) . ')';
        },
        'require' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            if ($arg->{is_version_string}) {
                # require VERSION
                return 'p5pkg["Perlito5"]["test_perl_version"]([' 
                        . Perlito5::JavaScript2::to_str( $self->{arguments}[0] )
                    . '], ' . Perlito5::JavaScript2::to_context($wantarray) . ')';
            }
            # require FILE
            'p5pkg["Perlito5::Grammar::Use"]["require"]([' 
                . Perlito5::JavaScript2::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 1 : 0) 
            . '], ' . Perlito5::JavaScript2::to_context($wantarray) . ')';
        },
        'select' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5pkg["CORE"]["select"]([' 
                . ( $self->{arguments}[0]{bareword}
                  ? Perlito5::JavaScript2::to_str( $self->{arguments}[0] )
                  : $self->{arguments}[0]->emit_javascript2( $level, 'scalar' ) )
            . '])';
        },
        'prefix:<$>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            return 'p5scalar_deref(' 
                    . $arg->emit_javascript2( $level ) . ', '
                    . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ', '
                    . '""'      # autovivification type
                    . ')';
        },
        'prefix:<@>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            my $s = 'p5array_deref(' 
                  . Perlito5::JavaScript2::emit_javascript2_autovivify( $arg, $level, 'array' ) . ', '
                  . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                  . ')';
            return $wantarray eq 'scalar'
                ? "p5num($s)"
                : $s;
        },
        'prefix:<$#>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return '(p5array_deref(' 
                    . Perlito5::JavaScript2::emit_javascript2_autovivify( $arg, $level, 'array' ) . ', '
                    . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                    . ').length - 1)';
        },
        'prefix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return 'p5hash_deref(' 
                    . Perlito5::JavaScript2::emit_javascript2_autovivify( $arg, $level, 'hash' ) . ', '
                    . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                    . ')';
        },
        'prefix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            'p5cget_by_name(' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_javascript2($level) . ')([])';
        },
        'circumfix:<[ ]>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new p5ArrayRef(' . Perlito5::JavaScript2::to_list( $self->{arguments} ) . '))';
        },
        'circumfix:<{ }>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new p5HashRef(' . Perlito5::JavaScript2::to_list( $self->{arguments}, $level, 'hash' ) . '))';
        },
        'prefix:<\\>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Apply') ) {
                if ( $arg->{code} eq 'prefix:<@>' ) {
                    return '(new p5ArrayRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->{code} eq 'prefix:<%>' ) {
                    return '(new p5HashRef(' . $arg->emit_javascript2($level) . '))';
                }
                # if ( $arg->{code} eq '*' ) {
                #     # TODO
                #     return '(new p5GlobRef(' . $arg->emit_javascript2($level) . '))';
                # }
                if ( $arg->{code} eq 'circumfix:<( )>' ) {
                    # \( @x )
                    return 'p5_list_of_refs(' . Perlito5::JavaScript2::to_list( $arg->{arguments} ) . ')';
                }
                if ( $arg->{code} eq 'prefix:<&>' ) {
                    return 'p5code_lookup_by_name(' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->{arguments}->[0]->emit_javascript2($level) . ')';
                }
            }
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return '(new p5ArrayRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new p5HashRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new p5GlobRef(' . $arg->emit_javascript2($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ( $arg->{namespace} ) {
                        return 'p5pkg[' . Perlito5::JavaScript2::escape_string($arg->{namespace} ) . '].' . $arg->{name};
                    }
                    else {
                        return Perlito5::JavaScript2::pkg() . '.' . $arg->{name};
                    }
                }
            }
            return '(new p5ScalarRef(' . $arg->emit_javascript2($level) . '))';
        },

        'postfix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5postincr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::JavaScript2::get_label();
                return Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5incr_(' . $tmp . ');',
                            'return ' . $tmp,
                )
            }
            '(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')++';
        },
        'postfix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];

            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5postdecr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::JavaScript2::get_label();
                return Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5decr_(' . $tmp . ');',
                            'return ' . $tmp,
                )
            }

            '(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')--';
        },
        'prefix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5incr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::JavaScript2::get_label();
                return Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5incr_(' . $tmp . ');',
                            'return ' . $arg->emit_javascript2($level+1),
                )
            }
            '++(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
        },
        'prefix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];

            if  (   $arg->isa( 'Perlito5::AST::Index')
                ||  $arg->isa( 'Perlito5::AST::Lookup') 
                ||  $arg->isa( 'Perlito5::AST::Call') 
                )
            {
                return $arg->emit_javascript2($level+1, 0, 'p5decr');
            }
            if  (   $arg->isa( 'Perlito5::AST::Var')
                &&  $arg->{sigil} eq '$'
                )
            {
                my $tmp  = Perlito5::JavaScript2::get_label();
                return Perlito5::JavaScript2::emit_wrap_javascript2($level, 'scalar', 
                            'var ' . $tmp . ' = ' . $arg->emit_javascript2($level) . ';',
                            $arg->emit_javascript2($level) . ' = p5decr_(' . $tmp . ');',
                            'return ' . $arg->emit_javascript2($level+1),
                )
            }

            '--(' . join( ' ', map( $_->emit_javascript2, @{ $self->{arguments} } ) ) . ')';
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
                return 'p5list_replicate('
                           . $self->{arguments}->[0]->emit_javascript2($level, 'list') . ','
                           . Perlito5::JavaScript2::to_num($self->{arguments}->[1], $level) . ', '
                           . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 )
                        . ')'
            }
            'p5str_replicate('
                           . Perlito5::JavaScript2::to_str($self->{arguments}->[0], $level) . ','
                           . Perlito5::JavaScript2::to_num($self->{arguments}->[1], $level) . ')'
        },

        'list:<.>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(' . join( ' + ', map( Perlito5::JavaScript2::to_str($_), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::JavaScript2::to_list( $self->{arguments} );
        },
        'infix:<..>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'p5range(' . $self->{arguments}->[0]->emit_javascript2($level) . ', '
                              . $self->{arguments}->[1]->emit_javascript2($level) . ', '
                              . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ', '
                              . '"' . Perlito5::JavaScript2::get_label() . '"' . ', '
                              . '0'
                        . ')'
        },
        'infix:<...>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'p5range(' . $self->{arguments}->[0]->emit_javascript2($level) . ', '
                              . $self->{arguments}->[1]->emit_javascript2($level) . ', '
                              . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ', '
                              . '"' . Perlito5::JavaScript2::get_label() . '"' . ', '
                              . '1'
                        . ')'
        },
        'delete' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                my $v_js = $v->emit_javascript2();
                my $key_js = $arg->autoquote($arg->{index_exp})->emit_javascript2($level);
                my $suffix = ((  $v->isa('Perlito5::AST::Var') && $v->sigil eq '$') ? '' : '._hash_');

                return "((function (v,k) { var ret = v[k]; delete (v[k]); return ret;})(" . $v_js . $suffix . ',' . $key_js . "))";
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    return '(delete ' . $v->emit_javascript2() . '[' . $arg->{index_exp}->emit_javascript2($level) . '])';
                }
                return '(delete ' . $v->emit_javascript2() . '._array_[' . $arg->{index_exp}->emit_javascript2($level) . '])';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return '(delete ' . $arg->invocant->emit_javascript2() . '._hash_[' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_javascript2($level) . '])';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return '(delete ' . $arg->invocant->emit_javascript2() . '._array_[' . $arg->{arguments}->emit_javascript2($level) . '])';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                die 'TODO delete &code';
                # my $name = $arg->{name};
                # my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                # return 'p5pkg[' . Perlito5::JavaScript2::escape_string($namespace) . '].hasOwnProperty(' . Perlito5::JavaScript2::escape_string($name) . ')';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                die 'TODO delete &$code';
                # my $arg2 = $arg->{arguments}->[0];
                # return 'p5sub_exists(' . Perlito5::JavaScript2::to_str($arg2) . ', ' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ')';
            }
        },

        'scalar' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::JavaScript2::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::JavaScript2::to_bool( $self->{arguments}->[0] ) . ' ? ' . ( $self->{arguments}->[1] )->emit_javascript2( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_javascript2( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of my($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'state' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of state($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'our' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of our($x,$y)
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'local' => sub {
            my ($self, $level, $wantarray) = @_;
            # 'local ($x, $y[10])'
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'circumfix:<( )>' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5context(' . '[' . join( ', ', map( $_->emit_javascript2( $level, $wantarray ), @{ $self->{arguments} } ) ) . '], ' . ( $wantarray eq 'runtime' ? 'p5want' : $wantarray eq 'list' ? 1 : 0 ) . ')';
        },
        'infix:<=>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];
            return _emit_assignment_javascript2($parameters, $arguments, $level, $wantarray);
                    },

        'break' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("break", ""))'
            );
        },
        'next' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = '';
            $label = $self->{arguments}[0]{code} || "" if @{$self->{arguments}};
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("next", ' . Perlito5::JavaScript2::escape_string($label ) . '))'
            );
        },
        'last' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = '';
            $label = $self->{arguments}[0]{code} || "" if @{$self->{arguments}};
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("last", ' . Perlito5::JavaScript2::escape_string($label ) . '))'
            );
        },
        'redo' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = '';
            $label = $self->{arguments}[0]{code} || "" if @{$self->{arguments}};
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(new p5_error("redo", ' . Perlito5::JavaScript2::escape_string($label ) . '))'
            );
        },
        'return' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(' . Perlito5::JavaScript2::to_runtime_context( $self->{arguments}, $level+1 ) . ')'
            );
        },
        'goto' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            Perlito5::JavaScript2::emit_wrap_statement_javascript2(
                $level,
                $wantarray, 
                'throw(' . $self->{arguments}->[0]->emit_javascript2($level) . ')'
            );
        },

        'do' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # do BLOCK
                my $block = $arg->{stmts};
                return Perlito5::JavaScript2::emit_wrap_javascript2(
                    $level,
                    $wantarray, 
                    (Perlito5::JavaScript2::LexicalBlock->new( block => $block ))->emit_javascript2( $level + 1, $wantarray )
                )
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
            my $js = $ast->emit_javascript2( $level, $wantarray );
            return $js;
        },

        'eval' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;   # we can return() from inside eval

            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # eval block

                $eval = Perlito5::AST::Apply->new(
                            code => 'do',
                            arguments => [$arg]
                        )->emit_javascript2( $level + 1, $wantarray );
            }
            else {
                # eval string

                # retrieve the parse-time env

                my %vars;
                for my $var (@{ $self->{_scope}{block} }, @Perlito5::CAPTURES) {
                    if ( $var->{_decl} && $var->{_decl} ne 'global' ) {
                        $vars{$var->{_id}} = $var
                    }
                }

                # "$scope" contains the "my" declarations
                # scope only contains variables captured by the current subroutine,
                # and variables declared since the 'sub' started.

                my $scope = Perlito5::DumpToAST::dump_to_ast({'block' => [values(%vars)], }, {}, 's');
                my $scope_js = '(new p5ArrayRef(' . Perlito5::JavaScript2::to_list([$scope]) . '))';

                my $hash_hints = Perlito5::DumpToAST::dump_to_ast($self->{_hash_hints}, {}, 's');
                my $hash_hints_js = $hash_hints->emit_javascript2($level);

                $eval ='eval(p5pkg["Perlito5::JavaScript2::Runtime"].perl5_to_js([' 
                            . Perlito5::JavaScript2::to_str($arg) . ", "
                            . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ', '
                            . Perlito5::JavaScript2::escape_string($wantarray) . ', '
                            . ( 0 + $self->{_scalar_hints} ) . ', '
                            . $hash_hints_js . ', '
                            . $scope_js
                        . "]))";
            }

            # TODO - test return() from inside eval

            my $context = Perlito5::JavaScript2::to_context($wantarray);

            Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                ( $context eq 'p5want'
                  ? ()
                  : "var p5want = " . $context . ";",
                ),
                "var r;",
                'p5pkg["main"]["v_@"] = "";',
                "try {",
                    [ 'r = ' . $eval . "",
                    ],
                "}",
                "catch(err) {",
                 [
                   "if (err instanceof p5_error && (err.type == 'last' || err.type == 'redo' || err.type == 'next')) {",
                        [ 'throw(err)' ],
                   '}',
                   "else if ( err instanceof p5_error || err instanceof Error ) {",
                     [ 'p5pkg["main"]["v_@"] = err;',
                       'if (p5str(p5pkg["main"]["v_@"]).substr(-1, 1) != "\n") {',
                           [ # try to add a stack trace
                             'try {' . "",
                                 [ 'p5pkg["main"]["v_@"] = p5pkg["main"]["v_@"] + "\n" + err.stack + "\n";',
                                 ],
                             '}',
                             'catch(err) { }',
                           ],
                       '}',
                     ],
                   "}",
                   "else {",
                     [ "return(err);",
                     ],
                   "}",
                 ],
                "}",
                "return r;",
            );
        },

        'substr' => sub {
            my ($self, $level, $wantarray) = @_;
            my $length = $self->{arguments}->[2];
            if ( $length && $length->isa('Perlito5::AST::Int') && $length->{int} > 0 ) {
                return Perlito5::JavaScript2::to_str($self->{arguments}->[0]) 
                    . '.substr(' . Perlito5::JavaScript2::to_num($self->{arguments}->[1]) . ', ' 
                                 . Perlito5::JavaScript2::to_num($self->{arguments}->[2]) . ')'
            }
            my $arg_list = Perlito5::JavaScript2::to_list_preprocess( $self->{arguments} );
            my $arg_code = Perlito5::JavaScript2::to_list($arg_list);
            return 'CORE.substr(' 
                    . $arg_code . ', '
                    . Perlito5::JavaScript2::to_context($wantarray)
                 . ')';
        },
        'undef' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                my $arg = $self->{arguments}[0];
                if (  ref( $arg ) eq 'Perlito5::AST::Var'
                   && $arg->{sigil} eq '&'
                   )
                {
                    return '(delete p5pkg[' . Perlito5::JavaScript2::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::JavaScript2::escape_string($arg->{name} ) . '])';
                }
                return '('. _emit_assignment_javascript2(
                    $arg,
                    Perlito5::AST::Apply->new(
                        'arguments' => [],
                        'bareword' => 1,
                        'code' => 'undef',
                    ),
                    $level+1,
                    $wantarray,
                ) . ')';
            }
            return 'null';
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
                $invocant = 'p5code_lookup_by_name(' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg2->emit_javascript2($level) . ')';
            }
            elsif (  ref( $arg ) eq 'Perlito5::AST::Var' 
               && $arg->{sigil} eq '&'
               )
            {
                $invocant = 'p5pkg[' . Perlito5::JavaScript2::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::JavaScript2::escape_string($arg->{name} ) . ']';
            }
            else {
                $invocant = $arg->emit_javascript2($level, 'scalar');
            }
            '(' . $invocant . ' != null)' 
        },
        'shift' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_javascript2( $level ) . '.shift()'
            }
            if ($Perlito5::JavaScript2::is_inside_subroutine) {
                return 'List__.shift()';                          # shift @_
            }
            return 'p5pkg.main.List_ARGV.shift()';                # shift @ARGV
        },
        'pop' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_javascript2( $level ) . '.pop()'
            }
            if ($Perlito5::JavaScript2::is_inside_subroutine) {
                return 'List__.pop()';                          # pop @_
            }
            return 'p5pkg.main.List_ARGV.pop()';                # pop @ARGV
        },
        'unshift' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            return $v->emit_javascript2( $level ) . '.p5unshift(' . Perlito5::JavaScript2::to_list(\@arguments) . ')';
        },
        'push' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            return $v->emit_javascript2( $level ) . '.p5push(' . Perlito5::JavaScript2::to_list(\@arguments) . ')';
        },
        'tie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

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
            return 'p5tie_' . $meth . '(' . $v->emit_javascript2( $level ) . ', ' . Perlito5::JavaScript2::to_list(\@arguments) . ')';
        },
        'untie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

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
            return 'p5untie_' . $meth . '(' . $v->emit_javascript2( $level ) . ')';
        },
        'print' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_javascript2( $level );
            }
            else {
                $fun  = '"STDOUT"';
            }
            my $list = Perlito5::JavaScript2::to_list(\@in, $level);
            'p5pkg["Perlito5::IO"].print(' . $fun . ', ' . $list . ')';
        },
        'say' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_javascript2( $level );
            }
            else {
                $fun  = '"STDOUT"';
            }
            my $list = Perlito5::JavaScript2::to_list(\@in, $level);
            'p5pkg["Perlito5::IO"].say(' . $fun . ', ' . $list . ')';
        },
        'printf' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_javascript2( $level );
            }
            else {
                $fun  = '"STDOUT"';
            }
            my $list = Perlito5::JavaScript2::to_list(\@in, $level);
            'p5pkg["Perlito5::IO"].printf(' . $fun . ', ' . $list . ')';
        },
        'close' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'p5pkg["Perlito5::IO"].close(' . $fun->emit_javascript2( $level ) . ', [])';
        },
        'open' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            if (ref($fun) ne 'Perlito5::AST::Apply') {
                # doesn't look like STDERR or FILE; initialize the variable with a GLOB
                return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                    $fun->emit_javascript2( $level ) . ' = CORE.bless([ {file_handle : {id : null}}, "GLOB" ]);',
                    'return CORE.open(' . Perlito5::JavaScript2::to_list( $self->{arguments}, $level ) . ')'
                );
            }
            else {
                return 'CORE.open(' . Perlito5::JavaScript2::to_list( $self->{arguments}, $level ) . ')'
            }
        },
        'chomp' => sub {
            my ($self, $level, $wantarray) = @_;
            # TODO - chomp assignment: chomp($answer = <STDIN>)
            my $v  = $self->{arguments}[0];
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                'var r = p5chomp(' . $v->emit_javascript2( $level ) . ');',
                $v->emit_javascript2( $level ) . ' = r[1];',
                'return r[0]',
            );
        },
        'chop' => sub {
            my ($self, $level, $wantarray) = @_;
            my $v  = $self->{arguments}[0];
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                'var r = p5chop(' . $v->emit_javascript2( $level ) . ');',
                $v->emit_javascript2( $level ) . ' = r[1];',
                'return r[0]',
            );
        },
        'read' => sub {
            my ($self, $level, $wantarray) = @_;
            # read FILEHANDLE,SCALAR,LENGTH,OFFSET
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            my $scalar = shift(@in);
            my $length = shift(@in);
            return Perlito5::JavaScript2::emit_wrap_javascript2($level, $wantarray,
                'var r = p5pkg["Perlito5::IO"].read(' . $fun->emit_javascript2( $level ) . ', [' . $length->emit_javascript2( $level ) . ']);',
                $scalar->emit_javascript2( $level ) . ' = r[1];',
                'return r[0]',
            );
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
                       'namespace' => '',
                   }, 'Perlito5::AST::Apply');
            return 'CORE.readline(['
                        . $fun->emit_javascript2( $level )
                . '], '
                . Perlito5::JavaScript2::to_context($wantarray)
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
            my $list = Perlito5::JavaScript2::to_list(\@in, $level);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5map(' . Perlito5::JavaScript2::pkg() . ', '
                    . 'function (p5want) {' . "\n"
                    . Perlito5::JavaScript2::tab($level+1) . (Perlito5::JavaScript2::LexicalBlock->new( block => $fun ))->emit_javascript2( $level + 1, $wantarray ) . "\n"
                    . Perlito5::JavaScript2::tab($level) . '}, '
                    .   $list
                    . ')';
        },
        'grep' => sub {
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
            my $list = Perlito5::JavaScript2::to_list(\@in, $level);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            'p5grep(' . Perlito5::JavaScript2::pkg() . ', '

                    . 'function (p5want) {' . "\n"
                    . Perlito5::JavaScript2::tab($level+1) . (Perlito5::JavaScript2::LexicalBlock->new( block => $fun ))->emit_javascript2( $level + 1, $wantarray ) . "\n"
                    . Perlito5::JavaScript2::tab($level) . '}, '

                    .   $list
                    . ')';
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
                $fun =
                      'function (p5want) {' . "\n"
                    . Perlito5::JavaScript2::tab($level+1) . (Perlito5::JavaScript2::LexicalBlock->new( block => $fun->{stmts} ))->emit_javascript2( $level + 1, $wantarray ) . "\n"
                    . Perlito5::JavaScript2::tab($level) . '}'
            }
            else {
                $fun = 'null';
            }
            $list = Perlito5::JavaScript2::to_list(\@in, $level);

            'p5sort(' . Perlito5::JavaScript2::pkg() . ', '
                    .   $fun . ', '
                    .   $list
                    . ')';
        },
        'infix:<//>' => sub { 
            my ($self, $level, $wantarray) = @_;
            'p5defined_or' . '('
                . $self->{arguments}->[0]->emit_javascript2($level, 'scalar') . ', '
                . Perlito5::JavaScript2::emit_function_javascript2($level, $wantarray, $self->{arguments}->[1]) 
                . ')'
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
                    $v->{sigil} = '%';
                    return '(' . $v->emit_javascript2() . ').hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . ')';
                }
                return '(' . $v->emit_javascript2() . ')._hash_.hasOwnProperty(' . $arg->autoquote($arg->{index_exp})->emit_javascript2($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    return '(' . $v->emit_javascript2() . ').hasOwnProperty(' . $arg->{index_exp}->emit_javascript2($level) . ')';
                }
                return '(' . $v->emit_javascript2() . ')._array_.hasOwnProperty(' . $arg->{index_exp}->emit_javascript2($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return Perlito5::JavaScript2::emit_javascript2_autovivify( $arg->invocant, $level, 'hash' ) . '._hash_.hasOwnProperty(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_javascript2($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return Perlito5::JavaScript2::emit_javascript2_autovivify( $arg->invocant, $level, 'array' ) . '._array_.hasOwnProperty(' . $arg->{arguments}->emit_javascript2($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                # TODO exist() + 'my sub'
                my $name = $arg->{name};
                my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                return 'p5pkg[' . Perlito5::JavaScript2::escape_string($namespace) . '].hasOwnProperty(' . Perlito5::JavaScript2::escape_string($name) . ')';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2 = $arg->{arguments}->[0];
                return 'p5sub_exists(' . Perlito5::JavaScript2::to_str($arg2) . ', ' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ')';
            }
        },

        'prototype' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            return 'p5sub_prototype(' . $arg->emit_javascript2() . ', ' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME) . ')';
        },
        'split' => sub {
            my ($self, $level, $wantarray) = @_;
            my @js;
            my $arg = $self->{arguments}->[0];
            if ( $arg
              && $arg->isa('Perlito5::AST::Apply')
              && $arg->{code} eq 'p5:m'
            ) {
                # first argument of split() is a regex
                push @js, 'new RegExp('
                        . $arg->{arguments}->[0]->emit_javascript2() . ', '
                        . Perlito5::JavaScript2::escape_string($arg->{arguments}->[1]->{buf})
                    . ')';
                shift @{ $self->{arguments} };
            }
            return 'CORE.split('
                . '[' . join( ', ',
                    @js,
                    map( $_->emit_javascript2, @{ $self->{arguments} } ) )
                . '], '
                . Perlito5::JavaScript2::to_context($wantarray)
            . ')';
        },
    );

    sub emit_javascript2 {
        my ($self, $level, $wantarray) = @_;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_javascript2( $level );
        }
        my $apply = $self->op_auto();
        if ($apply) {
            return $apply->emit_javascript2( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_javascript2
                for @{$self->{arguments}};

            if ( ref($code) eq 'Perlito5::AST::Apply' && $code->code eq "prefix:<&>") {
                # &$c()

                my $arg   = $self->{code}{arguments}->[0];
                $code = 'p5code_lookup_by_name(' . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_javascript2($level) . ')';

                return $code . '([' . join(',', @args) . '])';
            }

            return '(' . $self->{code}->emit_javascript2( $level ) . ')([' . join(',', @args) . '])';
        }

        return $emit_js{$code}->($self, $level, $wantarray)
            if exists $emit_js{$code};

        if (exists $Perlito5::JavaScript2::op_infix_js_str{$code}) {
            return '(' 
                . join( $Perlito5::JavaScript2::op_infix_js_str{$code}, map { Perlito5::JavaScript2::to_str($_, $level) } @{$self->{arguments}} )
                . ')'
        }
        if (exists $Perlito5::JavaScript2::op_infix_js_num{$code}) {
            return '(' 
                . join( $Perlito5::JavaScript2::op_infix_js_num{$code}, map { Perlito5::JavaScript2::to_num($_, $level) } @{$self->{arguments}} )
                . ')'
        }
        if (exists $Perlito5::JavaScript2::op_prefix_js_str{$code}) {
            return $Perlito5::JavaScript2::op_prefix_js_str{$code} . '(' 
                . Perlito5::JavaScript2::to_str($self->{arguments}[0])
                . ')'
        }

        if ($self->{namespace}) {
            if ($self->{namespace} eq 'JS') {
                if ($code eq 'inline') {
                    if ( $self->{arguments}->[0]->isa('Perlito5::AST::Buf') ) {
                        # JS::inline('var x = 123')
                        return $self->{arguments}[0]{buf};
                    }
                    else {
                        die "JS::inline needs a string constant";
                    }
                }
            }
            if ($self->{namespace} eq 'Perlito5') {
                # if ($self->{code} eq 'nop') {
                #     return "";
                # }
                if ($code eq 'eval_ast') {
                    $self->{namespace} = 'Perlito5::JavaScript2::Runtime';
                }
            }
            $code = 'p5pkg[' . Perlito5::JavaScript2::escape_string($self->{namespace} ) . '].' . $code;
        }
        else {
            $code = Perlito5::JavaScript2::pkg() . '.' . $code
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
            elsif ( exists $Perlito5::PACKAGES->{$name} ) {
                # bareword is a package name
                return Perlito5::JavaScript2::escape_string($name);
            }
            else {
                # this subroutine was never declared
                if ($self->{bareword}) {
                    # TODO: allow barewords where a glob is expected: open FILE, ...

                    # bareword doesn't call AUTOLOAD
                    return Perlito5::JavaScript2::escape_string( 
                            ($self->{namespace} ? $self->{namespace} . '::' : "") . $name 
                        );
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

            my $close = ']';

            my $optional = 0;
            while (length $sig) {
                my $c = substr($sig, 0, 1);
                if ($c eq ';') {
                    $optional = 1;
                }
                elsif ($c eq '$' || $c eq '_') {
                    push @out, shift(@in)->emit_javascript2( $level + 1, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '+') {

                    # The "+" prototype is a special alternative to "$" that will act like
                    # "\[@%]" when given a literal array or hash variable, but will otherwise
                    # force scalar context on the argument.
                    if (@in || !$optional) {
                        my $in = shift(@in);
                        if (  (  $in->isa('Perlito5::AST::Apply')
                              && $in->{code} eq 'prefix:<@>'
                              )
                           || (  $in->isa('Perlito5::AST::Var')
                              && $in->sigil eq '@'
                              )
                           || (  $in->isa('Perlito5::AST::Apply')
                              && $in->{code} eq 'prefix:<%>'
                              )
                           || (  $in->isa('Perlito5::AST::Var')
                              && $in->sigil eq '%'
                              )
                           )
                        {
                            push @out, $in->emit_javascript2( $level + 1, 'list' );
                        }
                        else {
                            push @out, $in->emit_javascript2( $level + 1, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '@') {
                    $close = '].concat(' . Perlito5::JavaScript2::to_list(\@in, $level + 1) . ')'
                        if @in || !$optional;
                    @in = ();
                }
                elsif ($c eq '&') {
                    push @out, shift(@in)->emit_javascript2( $level + 1, 'scalar' );
                }
                elsif ($c eq '*') {
                    if (@in || !$optional) {
                        my $arg = shift @in;
                        if ($arg->{bareword}) {
                            push @out, Perlito5::JavaScript2::escape_string($arg->{code});
                        }
                        else {
                            push @out, $arg->emit_javascript2( $level + 1, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript2( $level + 1, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_javascript2( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_javascript2( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 6) eq '\\[$@%]') {
                        $sig = substr($sig, 5);
                        push @out, shift(@in)->emit_javascript2( $level + 1, 'list' ) if @in || !$optional;
                    }
                }
                $sig = substr($sig, 1);
            }

            return $code . '([' . join(', ', @out) . $close . ', '
                        . Perlito5::JavaScript2::to_context($wantarray)
                . ')';
        }

        my $arg_list = Perlito5::JavaScript2::to_list_preprocess( $self->{arguments} );

        my $arg_code = 
            $self->{code} eq 'scalar'      # scalar() is special
            ?   '['
              .   join(', ', map( $_->emit_javascript2($level), @$arg_list ))
              . ']'
            : Perlito5::JavaScript2::to_list($arg_list, $level);


        if ( $may_need_autoload ) {
            # p5cget(namespace, name)(list, p5want)
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            return 'p5cget('
                    . Perlito5::JavaScript2::escape_string($namespace) . ', '
                    . Perlito5::JavaScript2::escape_string($name) . ')('
                    . $arg_code . ', '
                    . Perlito5::JavaScript2::to_context($wantarray)
                 . ')';

        }

        $code . '('
                . $arg_code . ', '
                . Perlito5::JavaScript2::to_context($wantarray)
              . ')';

    }

    sub emit_javascript2_set_list {
        my ($self, $level, $list) = @_;
        my $code = $self->{code};
        if ( $self->code eq 'undef' ) {
            return $list . '.shift()' 
        }
        if ( $self->code eq 'prefix:<$>' ) {
            return 'p5scalar_deref_set(' 
                . Perlito5::JavaScript2::emit_javascript2_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . ', '
                . $list . '.shift()'  . ', '
                . Perlito5::JavaScript2::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        if ($code eq 'my' || $code eq 'our' || $code eq 'state' || $code eq 'local') {
            return join( '; ',
                     map { $_->emit_javascript2_set_list( $level, $list ) }
                         @{ $self->{arguments} }
                   );
        }
        die "not implemented: assign to ", $self->code;
    }

    sub emit_javascript2_get_decl {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'my' || $code eq 'our' || $code eq 'state' || $code eq 'local') {
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
            return ( map  +( $_->emit_javascript2_get_decl ), 
                          @{ $self->{arguments} }
                   )
                if $self->{arguments};
        }
        return ()
    }
    sub emit_javascript2_has_regex {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'p5:m' || $code eq 'p5:s' || $code eq 'infix:<=~>' || $code eq 'infix:<!~>') {
            return 1;
        }
        return ()
    }
}

1;


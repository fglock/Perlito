use v6;

class Python {
    sub tab($level) { "    " x $level }
}

class MiniPerl6::Python::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;

    my $ident;
    my @anon_block;
    sub push_stmt_python($block) { 
        push @anon_block, $block; 
    }
    sub get_ident_python {
        $ident = $ident + 1;
        return $ident;
    }
    method has_my_decl {
        for @.block -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                return 1;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                return 1;
            }
        }
        return 0;
    }

    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if !(@.block) {
            push @.block, Val::Undef.new();
        }

        my @s;
        my @tmp;
        for @anon_block -> $stmt {
            @tmp.push( $stmt );
        }

        my $has_decl = [];
        my $block = [];
        for @.block -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                push $has_decl, $decl;
            }
            elsif $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'has' ) {
                push $has_decl, $decl;
            }
            else {
                push @($block), $decl;
            }
        }
        if @($has_decl) {
            my @decl;
            my $self_var = Var.new( 'name' => 'self', 'sigil' => '$', 'twigil' => '' );
            for @($has_decl) -> $decl {
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    # TODO 
                    push @decl, Bind.new(
                                    parameters => Call.new(
                                                    'arguments' => [],
                                                    'hyper' => '',
                                                    'invocant' => Proto.new( name => 'self' ),
                                                    'method' => 'xyz'
                                                ),
                                    arguments => Val::Int.new( int => 0 ),
                                );
                }
                if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'has' ) {
                    # TODO 
                    push @decl, ($decl.parameters);
                }
            }
            push @decl, $self_var;
            push @s, (Method.new(
                        name  => '__init__', 
                        block => @decl,
                        sig   => Sig.new(
                                    'invocant' => $self_var,
                                    'named' => {},
                                    'positional' => []
                                )
                    )).emit_python_indented( $level );
        }

        for @($block) -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                push @s, $decl.emit_python_init( $level );
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                push @s, ($decl.parameters).emit_python_init( $level );
            }
        }

        my $last_statement;
        if $.needs_return {
            $last_statement = pop @($block);
        }

        for @($block) -> $stmt {
            @anon_block = [];
            my $s2 = $stmt.emit_python_indented($level);
            for @anon_block -> $stmt {
                @s.push( $stmt.emit_python_indented( $level ) );
            }
            push @s, $s2;
        }

        if $.needs_return && $last_statement {
            @anon_block = [];
            my $s2;
            if $last_statement.isa( 'If' ) {
                my $cond            = $last_statement.cond;
                my $has_otherwise   = $last_statement.otherwise ?? 1 !! 0;
                my $body_block      = 
                    MiniPerl6::Python::LexicalBlock.new( block => ($last_statement.body), needs_return => 1 );
                my $otherwise_block = 
                    MiniPerl6::Python::LexicalBlock.new( block => ($last_statement.otherwise), needs_return => 1 );

                if $body_block.has_my_decl {
                    $body_block = Return.new( result => Do.new( block => ($last_statement.body) ) );
                }
                if $has_otherwise && $otherwise_block.has_my_decl {
                    $otherwise_block = Return.new( result => Do.new( block => ($last_statement.otherwise) ) );
                }

                $s2 = Python::tab($level) ~ 'if ' ~ $cond.emit_python ~ ":\n" 
                    ~ $body_block.emit_python_indented( $level + 1 );
                if ( $has_otherwise ) {
                    $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "else:\n" 
                            ~ $otherwise_block.emit_python_indented($level+1);
                }
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                $s2 = $last_statement.emit_python_indented( $level );
            }
            else {
                $s2 = Python::tab($level) ~ 'return ' ~ $last_statement.emit_python
            }

            for @anon_block -> $stmt {
                @s.push( $stmt.emit_python_indented( $level ) );
            }
            @s.push( $s2 ); 
        }

        @anon_block = @tmp;
        return @s.join( "\n" );
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        Python::tab($level) ~ 'class ' ~ $.name ~ ":\n" ~ 
            $block.emit_python_indented($level + 1) ~ "\n"
    }
}

class Val::Int {
    has $.int;
    method emit_python { $.int }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.int 
    }
}

class Val::Bit {
    has $.bit;
    method emit_python { $.bit }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.bit 
    }
}

class Val::Num {
    has $.num;
    method emit_python { $.num }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.num 
    }
}

class Val::Buf {
    has $.buf;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ '"""' ~ $.buf ~ '"""' 
    }
}

class Val::Undef {
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 'mp6_Undef()'
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.class.perl ~ '(' ~ %.fields.perl ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $needs_interpolation = 0;
        for @.array1 -> $item {
            if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' )
            {
                $needs_interpolation = 1;
            }
        }
        if $needs_interpolation {
            my @block;
            my $temp_array = Var.new( 
                                'name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => '' );
            my $input_array = Var.new( 
                                'name' => 'b', 'namespace' => '', 'sigil' => '@', 'twigil' => '' );
            push @block, Decl.new( 
                            'decl' => 'my',
                            'type' => '',
                            'var'  => $temp_array
                        );
            my $index = 0;
            for @.array1 -> $item {
                if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                    || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' )
                {
                    push @block, Call.new(
                                    'method' => 'extend',
                                    'arguments' => [ 
                                        Index.new( obj => $input_array, index_exp => Val::Int.new( int => $index ) )
                                    ],
                                    'hyper' => '',
                                    'invocant' => $temp_array
                                );
                }
                else {
                    push @block, Call.new(
                                    'method' => 'push',
                                    'arguments' => [ 
                                        Index.new( obj => $input_array, index_exp => Val::Int.new( int => $index ) ) 
                                    ],
                                    'hyper' => '',
                                    'invocant' => $temp_array
                                );
                }
                $index = $index + 1;
            }
            push @block, $temp_array;
            my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
            # generate an anonymous sub in the current block
            MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                    Sub.new( 
                        name  => $label, 
                        block => @block,
                        sig   => Sig.new( invocant => undef, positional => [ $input_array ], named => {} ) 
                    )
                );
            # call the anonymous sub
            return Python::tab($level) 
                ~ $label ~ "(mp6_Array([" ~ (@.array1.>>emit_python).join(', ') ~ "]))";
        }
        else {
            Python::tab($level)  
                ~ 'mp6_Array([' ~ (@.array1.>>emit_python).join(', ') ~ '])';
        }
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $fields = @.hash1;
        my @dict;
        for @$fields -> $field { 
            push @dict, (($field[0]).emit_python ~ ':' ~ ($field[1]).emit_python);
        }; 
        Python::tab($level) ~ 
            '{' ~ @dict.join(', ') ~ '}';
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $fields = @.fields;
        my $str = '';
        for @$fields -> $field { 
            $str = $str ~ ($field[0]).emit_python ~ ' = ' ~ ($field[1]).emit_python ~ ',';
        }; 
        Python::tab($level) ~ 
            $.class ~ '( ' ~ $str ~ ' )';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '.index(' ~ $.index_exp.emit_python ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '[' ~ $.index_exp.emit_python ~ ']';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table = {
            '$' => '',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        };
        return Python::tab($level) ~ (
               ( $.twigil eq '.' )
            ?? ( 'self.' ~ $.name )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name )
                )
            )
    };
    method name {
        $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if $.parameters.isa( 'Index' ) {
            return Python::tab($level)  
                ~ ($.parameters.obj).emit_python ~ '.set('
                    ~ ($.parameters.index_exp).emit_python ~ ', '
                    ~ $.arguments.emit_python ~ ')'
        }
        Python::tab($level)  
            ~ $.parameters.emit_python ~ ' = ' ~ $.arguments.emit_python;
    }
}

class Proto {
    has $.name;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.name        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $invocant = $.invocant.emit_python;
        if $.method eq 'new' {
            # TODO
        }
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'chars')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
            	return "map(lambda: Main." ~ $.method ~ "( self, " ~ (@.arguments.>>emit_python).join(', ') ~ ') , ' ~ $invocant ~ ")\n";
            }
            else {
                return "Main." ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
            }
        };

        my $meth = $.method;
        if $meth eq 'postcircumfix:<( )>' {
            return Python::tab($level) ~ 
                $invocant ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        }
        
        my $call = '.' ~ $meth ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        if ($.hyper) {
            Python::tab($level) ~ 
                '[ map { $_' ~ $call ~ ' } @{ ' ~ $invocant ~ ' } ]';
        }
        else {
            Python::tab($level) ~ 
                $invocant ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $self.emit_python
    }
    method emit_python {
        
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_python ~ ').(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'self' };

        if $code eq 'say'        { return 'mp6_say('   ~ (@.arguments.>>emit_python).join(', ') ~ ')' } 
        if $code eq 'print'      { return 'mp6_print(' ~ (@.arguments.>>emit_python).join(', ') ~ ')' }
        if $code eq 'warn'       { return 'mp6_warn('  ~ (@.arguments.>>emit_python).join(', ') ~ ')' }

        if $code eq 'array'      { return '[' ~ (@.arguments.>>emit_python).join(' ')    ~ ']' };

        if $code eq 'prefix:<~>' { return '("" . ' ~ (@.arguments.>>emit_python).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return 'not ('  ~ (@.arguments.>>emit_python).join(' ')    ~ ')' };
        if $code eq 'prefix:<?>' { return 'not (not ('  ~ (@.arguments.>>emit_python).join(' ')    ~ '))' };

        if $code eq 'prefix:<$>' { return '${' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };
        if $code eq 'prefix:<@>' { return '@{' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return '(str('  ~ (@.arguments.>>emit_python).join(') + str(')  ~ '))' };
        if $code eq 'infix:<+>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') + mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_python).join(' - ')  ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_python).join(' * ')  ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_python).join(' / ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '('  ~ (@.arguments.>>emit_python).join(' and ') ~ ')' };
        if $code eq 'infix:<||>' { return '('  ~ (@.arguments.>>emit_python).join(' or ') ~ ')' };
        if $code eq 'infix:<eq>' { return '(str('  ~ (@.arguments.>>emit_python).join(') == str(')  ~ '))' };
        if $code eq 'infix:<ne>' { return '(str('  ~ (@.arguments.>>emit_python).join(') != str(')  ~ '))' };
 
        if $code eq 'infix:<==>' { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') == mp6_to_num(') ~ '))' };
        if $code eq 'infix:<!=>' { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') != mp6_to_num(') ~ '))' };
        if $code eq 'infix:<<>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') < mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<>>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') > mp6_to_num(')  ~ '))' };

        if $code eq 'ternary:<?? !!>' { 
            my $ast = 
                Do.new( 
                    block => [
                        If.new(
                            cond      => (@.arguments[0]),
                            body      => [ @.arguments[1] ],
                            otherwise => [ @.arguments[2] ],
                        ),
                    ]
                );
            return $ast.emit_python;
        }
        
        if $code eq 'substr' { 
            return (@.arguments[0]).emit_python ~ '[' 
                    ~ (@.arguments[1]).emit_python ~ ':' 
                    ~ (@.arguments[1]).emit_python ~ ' + ' ~ (@.arguments[2]).emit_python 
                ~ ']' 
        } 
        if $code eq 'index' { 
            return (@.arguments[0]).emit_python ~ '.index(' ~ (@.arguments[1]).emit_python ~ ')' 
        } 
        if $code eq 'shift' { 
            return (@.arguments[0]).emit_python ~ '.shift()' 
        } 

        $.code ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
    }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $self.emit_python 
    }
}

class Return {
    has $.result;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            'return ' ~ $.result.emit_python
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $has_body = @.body ?? 1 !! 0;
        my $has_otherwise = @.otherwise ?? 1 !! 0;
        my $body_block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        my $otherwise_block = MiniPerl6::Python::LexicalBlock.new( block => @.otherwise );

        if $body_block.has_my_decl {
            $body_block = Do.new( block => @.body );
        }
        if $has_otherwise && $otherwise_block.has_my_decl {
            $otherwise_block = Do.new( block => @.otherwise );
        }

        my $s = Python::tab($level) ~   'if ' ~ $.cond.emit_python ~ ":\n" 
            ~ $body_block.emit_python_indented( $level + 1 );
        if ( $has_otherwise ) {
            $s = $s ~ "\n"
                ~ Python::tab($level) ~ "else:\n" 
                    ~ $otherwise_block.emit_python_indented($level+1);
        }
        return $s;
    }
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $body_block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        if $body_block.has_my_decl {
            $body_block = Do.new( block => @.body );
        }
        if $.init && $.continue {
            die "not implemented (While)"
            #    'for ( '
            # ~  ( $.init     ?? $.init.emit_             ~ '; '  !! '; ' )
            # ~  ( $.cond     ?? 'f_bool(' ~ $.cond.emit_ ~ '); ' !! '; ' )
            # ~  ( $.continue ?? $.continue.emit_         ~ ' '   !! ' '  )
        }
        Python::tab($level)
            ~ 'while ' ~ $.cond.emit_python ~ ":\n"
                ~ $body_block.emit_python_indented( $level + 1 );
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $body_block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        if $body_block.has_my_decl {
            $body_block = Do.new( block => @.body );
        }
        Python::tab($level)
            ~ 'for ' ~ $.topic.emit_python ~ " in " ~ $.cond.emit_python ~ ":\n"
                ~ $body_block.emit_python_indented( $level + 1 );
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.name;
        Python::tab($level)
            ~ ( ( $decl eq 'has' )
            ?? ( '' )
            !! $.var.emit_python );
    }
    method emit_python_init( $level ) {
        if $.decl eq 'my' {
            my $str = Python::tab($level) ~ ($.var).emit_python ~ ' = ';
            if ($.var).sigil eq '%' {
                $str = $str ~ '{}';
            }
            elsif ($.var).sigil eq '@' {
                $str = $str ~ 'mp6_Array([])';
            }
            else {
                $str = $str ~ 'mp6_Undef()';
            }
            return $str;
        }
        else {
            die "not implemented: Decl '" ~ $.decl ~ "'";
        }
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_python {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
    method invocant {
        $.invocant
    };
    method positional {
        $.positional
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my @args;
        @args.push( $invocant.emit_python );
        for @$pos -> $field { 
            @args.push( $field.emit_python );
        };
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        Python::tab($level) ~ 'def ' ~ $.name ~ "(" ~ @args.join(", ") ~ "):\n" 
            ~ $block.emit_python_indented($level + 1) 
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if ( $.name eq '' ) {
            my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
            # generate an anonymous sub in the current block
            MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                    Sub.new( 
                        name  => $label, 
                        block => @.block,
                        sig   => $.sig 
                    )
                );
            # return a ref to the anonymous sub
            return Python::tab($level) ~ $label;
        }

        my $sig = $.sig;
        my $pos = $sig.positional;
        my @args;
        for @$pos -> $field { 
            @args.push( $field.emit_python );
        };
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        Python::tab($level) ~ 'def ' ~ $.name ~ "(" ~ @args.join(", ") ~ "):\n" 
            ~ $block.emit_python_indented($level + 1) 
    }
}

class Do {
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        # generate an anonymous sub in the current block
        MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                Sub.new( 
                    name  => $label, 
                    block => @.block,
                    sig   => Sig.new( invocant => undef, positional => [], named => {} ) 
                )
            );
        # call the anonymous sub
        return Python::tab($level) ~ $label ~ "()";
    }
}

class Use {
    has $.mod;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) 
            ~ 'from ' ~ $.mod ~ 'import *'
    }
}

=begin

=head1 NAME

MiniPerl6::Python::Emit - Code generator for MiniPerl6-in-Python

=head1 SYNOPSIS

    $program.emit_python  # generated Python code

=head1 DESCRIPTION

This module generates Python code for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


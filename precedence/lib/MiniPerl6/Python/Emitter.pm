use v6;

class Python {
    sub tab($level) { 
        my $s = '';
        my $count = $level;
        while $count > 0 {
            $s = $s ~ "    ";
            $count = $count - 1; 
        }
        return $s;
    }
}

class MiniPerl6::Python::AnonSub {
    has $.name;
    has $.sig;
    has @.block;
    has $.handles_return_exception;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        for @$pos -> $field { 
            $args.push( $field.emit_python_name );
        };
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Python::tab($level) ~ "def f_" ~ $.name ~ "(" ~ $args.join(", ") ~ "):";
        for @($args) -> $field { 
            push @s, Python::tab($level+1) ~    $field ~ " = [" ~ $field ~ "]";
        }
        if $.handles_return_exception {
            push @s, Python::tab($level+1) ~    "try:";
            push @s,    $block.emit_python_indented($level + 2);
            push @s, Python::tab($level+1) ~    "except mp6_Return, r:";
            push @s, Python::tab($level+2) ~        "return r.value";
        }
        else {
            push @s,    $block.emit_python_indented($level + 1); 
        }
        return @s.join("\n");

    }
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

            # create accessors
            for @($has_decl) -> $decl {
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
                    push @s, Python::tab($level) ~ 'def f_' ~ $label ~ '(v_self):';
                    push @s, Python::tab($level+1) ~ 'return v_self.v_' ~ ($decl.var).name;
                    push @s, Python::tab($level) ~ "self.__dict__.update({'f_" ~ ($decl.var).name ~ "':f_" ~ $label ~ "})";
                }
                if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'has' ) {
                    my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
                    push @s, Python::tab($level) ~ 'def f_' ~ $label ~ '(v_self):';
                    push @s, Python::tab($level+1) ~ 'return v_self.v_' ~ (($decl.parameters).var).name;
                    push @s, Python::tab($level) ~ "self.__dict__.update({'f_" ~ (($decl.parameters).var).name ~ "':f_" ~ $label ~ "})";
                }
            }

        }

        for @($block) -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                push @s, Python::tab($level) ~ ($decl.var).emit_python_name ~ ' = [' ~ $decl.emit_python_init ~ ']';
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                push @s, Python::tab($level) ~ (($decl.parameters).var).emit_python_name ~ ' = [' ~ ($decl.parameters).emit_python_init ~ ']';
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

                if $body_block.has_my_decl() {
                    $body_block = Return.new( result => Do.new( block => ($last_statement.body) ) );
                }
                if $has_otherwise && $otherwise_block.has_my_decl() {
                    $otherwise_block = Return.new( result => Do.new( block => ($last_statement.otherwise) ) );
                }

                $s2 = Python::tab($level) ~ 'if mp6_to_bool(' ~ $cond.emit_python ~ "):\n" 
                    ~ $body_block.emit_python_indented( $level + 1 );
                if ( $has_otherwise ) {
                    $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "else:\n" 
                            ~ $otherwise_block.emit_python_indented($level+1);
                }
            }
            elsif $last_statement.isa( 'Bind' ) {
                $s2 = $last_statement.emit_python_indented( $level );
                $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "return " ~ ($last_statement.parameters).emit_python;
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                $s2 = $last_statement.emit_python_indented( $level );
            }
            else {
                $s2 = Python::tab($level) ~ "return " ~ $last_statement.emit_python;
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
        my @s;
        my $block = MiniPerl6::Python::LexicalBlock.new( block => @.body );
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        my $name = Main::to_go_namespace($.name);

        for @.body -> $decl {
            if $decl.isa('Use') {
                push @s, Python::tab($level) ~ 'from ' ~ Main::to_go_namespace($decl.mod) ~ ' import *'
            }
        }

        push @s, Python::tab($level)    ~   'try:';
        push @s, Python::tab($level+1)  ~       'type(' ~ $name ~ ")";
        push @s, Python::tab($level)    ~   'except NameError:';
        push @s, Python::tab($level+1)  ~       "__all__.extend(['" ~ $name ~ "', '" ~ $name ~ "_proto'])";
        push @s, Python::tab($level+1)  ~       'class ' ~ $name ~ ":";
        push @s, Python::tab($level+2)  ~           "def __init__(v_self, **arg):";
        push @s, Python::tab($level+3)  ~               "v_self.__dict__.update(arg)";
        push @s, Python::tab($level+2)  ~           "def __setattr__(v_self, k, v):";
        push @s, Python::tab($level+3)  ~               "v_self.__dict__[k] = v";
        push @s, Python::tab($level+2)  ~           "def f_isa(v_self, name):";
        push @s, Python::tab($level+3)  ~               "return name == '" ~ $.name ~ "'";
        push @s, Python::tab($level+2)  ~           "def __nonzero__(self):";
        push @s, Python::tab($level+3)  ~               "return 1";

        push @s, Python::tab($level+2)  ~           "def __getattr__(self, attr):";
        push @s, Python::tab($level+3)  ~               "if attr[0:2] == 'v_':";
        push @s, Python::tab($level+4)  ~                   "return mp6_Undef()";
        push @s, Python::tab($level+3)  ~               "raise AttributeError(attr)";

        push @s, Python::tab($level+1)  ~       $name ~ "_proto = " ~ $name ~ "()"; 
        push @s, Python::tab($level+1)  ~       "__builtin__." ~ $name ~ " = " ~ $name ~ ""; 
        push @s, Python::tab($level+1)  ~       "__builtin__." ~ $name ~ "_proto = " ~ $name ~ "_proto"; 
        push @s, Python::tab($level)    ~   'def ' ~ $label ~ "():";
        push @s, Python::tab($level+1)  ~       'self = ' ~ $name;
        push @s,    $block.emit_python_indented($level + 1);
        push @s, Python::tab($level)    ~   $label ~ "()";
        return @s.join( "\n" );
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
        Python::tab($level) ~ '"' ~ Main::javascript_escape_string($.buf) ~ '"' 
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
            $.class.emit_python ~ '(' ~ %.fields.emit_python ~ ')';
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
                    MiniPerl6::Python::AnonSub.new( 
                        name  => $label, 
                        block => @block,
                        sig   => Sig.new( invocant => undef, positional => [ $input_array ], named => {} ),
                        handles_return_exception => 1,
                    )
                );
            # call the anonymous sub
            return Python::tab($level) 
                ~ "f_" ~ $label ~ "(mp6_Array([" ~ (@.array1.>>emit_python).join(', ') ~ "]))";
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
            'mp6_Hash({' ~ @dict.join(', ') ~ '})';
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
        my @str;
        for @$fields -> $field { 
            push @str, "v_" ~ ($field[0]).buf ~ '=' ~ ($field[1]).emit_python;
        }
        Python::tab($level) ~ 
            Main::to_go_namespace($.class) ~ '(' ~ @str.join(', ') ~ ')';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '.f_index(' ~ $.index_exp.emit_python ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python ~ '.f_lookup(' ~ $.index_exp.emit_python ~ ')';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => 'Code_',
    };
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        return Python::tab($level) ~ (
               ( $.twigil eq '.' )
            ?? ( 'v_self[0].v_' ~ $.name ~ '' )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH[0]' )
                !!   ( $table{$.sigil} ~ $.name ~ '[0]' )
                )
            )
    };
    method emit_python_name {
        return (
               ( $.twigil eq '.' )
            ?? ( 'v_self[0].v_' ~ $.name )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name )
                )
            )
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if $.parameters.isa( 'Index' ) {
            return Python::tab($level)  
                ~ ($.parameters.obj).emit_python ~ '.f_set('
                    ~ ($.parameters.index_exp).emit_python ~ ', '
                    ~ $.arguments.emit_python ~ ')'
        }
        if $.parameters.isa( 'Lookup' ) {
            return Python::tab($level)  
                ~ ($.parameters.obj).emit_python ~ '.f_set('
                    ~ ($.parameters.index_exp).emit_python ~ ', '
                    ~ $.arguments.emit_python ~ ')'
        }
        if $.parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return Python::tab($level)  
                ~ ($.parameters.invocant).emit_python ~ ".__setattr__('v_" ~ $.parameters.method ~ "', " ~ $.arguments.emit_python ~ ")";
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
            Main::to_go_namespace($.name) ~ '_proto'
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $invocant = $.invocant.emit_python;
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'join')
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
            	return "map(lambda: Main." ~ $.method ~ "( v_self[0], " ~ (@.arguments.>>emit_python).join(', ') ~ ') , ' ~ $invocant ~ ")\n";
            }
            else {
                return "mp6_" ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
            }
        };

        my $meth = $.method;
        if $meth eq 'postcircumfix:<( )>' {
            return Python::tab($level) ~ 
                $invocant ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        }
        if     ( $meth eq 'values' ) 
            || ( $meth eq 'keys' )
        {
            return Python::tab($level) ~ $invocant ~ '.' ~ $meth ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        }
        if $meth eq 'chars' {
            return Python::tab($level) ~ "len(" ~ $invocant ~ ")";
        }
        
        my $call = 'f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        if ($.hyper) {
            Python::tab($level) ~ 'map(lambda x: x.' ~ $call ~ ', ' ~ $invocant ~ ')';
        }
        else {
            Python::tab($level) ~ $invocant ~ '.' ~ $call;
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
        
        # check that expressions don't overflow the Python parser stack
        if (@.arguments[1]).isa('Apply') {
            my $args2 = (@.arguments[1]).arguments;
            if ($args2[1]).isa('Apply') {
                $args2[1] = Do.new( block => [ $args2[1] ] );
            }
        }

        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_python ~ ').(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'v_self[0]' };
        if $code eq 'make'       { return "v_MATCH[0].__setattr__('v_capture', " ~ (@.arguments[0]).emit_python ~ ')' }
        if $code eq 'False'      { return 'False' };
        if $code eq 'True'       { return 'True' };

        if $code eq 'say'        { return 'mp6_say('   ~ (@.arguments.>>emit_python).join(', ') ~ ')' } 
        if $code eq 'print'      { return 'mp6_print(' ~ (@.arguments.>>emit_python).join(', ') ~ ')' }
        if $code eq 'warn'       { return 'mp6_warn('  ~ (@.arguments.>>emit_python).join(', ') ~ ')' }

        if $code eq 'array'      { return '[' ~ (@.arguments.>>emit_python).join(' ')      ~ ']' };

        if $code eq 'Int'        { return 'mp6_to_num(' ~ (@.arguments[0]).emit_python     ~ ')' };
        if $code eq 'Num'        { return 'mp6_to_num(' ~ (@.arguments[0]).emit_python     ~ ')' };

        if $code eq 'prefix:<~>' { return 'str('   ~ (@.arguments.>>emit_python).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { return 'not ('  ~ (@.arguments.>>emit_python).join(' ') ~ ')' };
        if $code eq 'prefix:<?>' { return 'not (not ('  ~ (@.arguments.>>emit_python).join(' ')    ~ '))' };

        if $code eq 'prefix:<$>' { return 'mp6_to_scalar(' ~ (@.arguments.>>emit_python).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_python).join(' ')    ~ ')' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_python).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return '(str('  ~ (@.arguments.>>emit_python).join(') + str(')  ~ '))' };
        if $code eq 'infix:<+>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') + mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_python).join(' - ')  ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_python).join(' * ')  ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_python).join(' / ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { 
            return '(mp6_to_bool(' ~ (@.arguments[0]).emit_python ~ ') '
                ~  'and mp6_to_bool(' ~ (@.arguments[1]).emit_python ~ '))' 
        }
        if $code eq 'infix:<||>' { 
            return '(mp6_to_bool(' ~ (@.arguments[0]).emit_python ~ ') '
                ~  'or mp6_to_bool(' ~ (@.arguments[1]).emit_python ~ '))' 
        }
        if $code eq 'infix:<eq>' { return '(str('  ~ (@.arguments.>>emit_python).join(') == str(')  ~ '))' };
        if $code eq 'infix:<ne>' { return '(str('  ~ (@.arguments.>>emit_python).join(') != str(')  ~ '))' };
 
        if $code eq 'infix:<==>' { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') == mp6_to_num(') ~ '))' };
        if $code eq 'infix:<!=>' { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') != mp6_to_num(') ~ '))' };
        if $code eq 'infix:<<>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') < mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<>>'  { return '(mp6_to_num('  ~ (@.arguments.>>emit_python).join(') > mp6_to_num(')  ~ '))' };

        if $code eq 'exists'     {
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_python ~ ').has_key(' ~ ($arg.index_exp).emit_python ~ ')';
            }
        }

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
            return 'mp6_index(' ~ (@.arguments[0]).emit_python ~ ', ' ~ (@.arguments[1]).emit_python ~ ')' 
        } 
        if $code eq 'shift'   { return (@.arguments[0]).emit_python ~ '.f_shift()' } 
        if $code eq 'pop'     { return (@.arguments[0]).emit_python ~ '.f_pop()'   } 
        if $code eq 'push'    { return (@.arguments[0]).emit_python ~ '.f_push('    ~ (@.arguments[1]).emit_python ~ ')' } 
        if $code eq 'unshift' { return (@.arguments[0]).emit_python ~ '.f_unshift(' ~ (@.arguments[1]).emit_python ~ ')' } 

        if $.namespace {
            return Main::to_go_namespace($.namespace) ~ '_proto.f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
        }
        'f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_python).join(', ') ~ ')';
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
            'raise mp6_Return(' ~ $.result.emit_python ~ ')';
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

        if $body_block.has_my_decl() {
            $body_block = Do.new( block => @.body );
        }
        if $has_otherwise && $otherwise_block.has_my_decl() {
            $otherwise_block = Do.new( block => @.otherwise );
        }

        my $s = Python::tab($level) ~   'if mp6_to_bool(' ~ $.cond.emit_python ~ "):\n" 
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
        if $body_block.has_my_decl() {
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
        if $body_block.has_my_decl() {
            # wrap the block into a call to anonymous subroutine 
            my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
            # generate an anonymous sub in the current block
            MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                    MiniPerl6::Python::AnonSub.new( 
                        name  => $label, 
                        block => @.body,
                        sig   => Sig.new( invocant => undef, positional => [ $.topic ], named => {} ),
                        handles_return_exception => 0,
                    )
                );
            return Python::tab($level) ~    'for ' ~ $.topic.emit_python_name ~ " in " ~ $.cond.emit_python ~ ":\n"
                ~  Python::tab($level+1) ~      "f_" ~ $label ~ "(" ~ $.topic.emit_python_name ~ ")";
        }
        Python::tab($level) ~   'for ' ~ $.topic.emit_python_name ~ " in " ~ $.cond.emit_python ~ ":\n"
        ~ Python::tab($level+1) ~     $.topic.emit_python_name ~ " = [" ~ $.topic.emit_python_name ~ "]\n"
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
    method emit_python_init {
        if ($.var).sigil eq '%' {
            return 'mp6_Hash({})';
        }
        elsif ($.var).sigil eq '@' {
            return 'mp6_Array([])';
        }
        else {
            return 'mp6_Undef()';
        }
        return '';
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_python {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
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
        my $args = [];
        my $default_args = [];
        my $meth_args = [];
        $meth_args.push( $invocant.emit_python_name );
        for @$pos -> $field { 
            my $arg = $field.emit_python_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=mp6_Undef()' );
            $meth_args.push( $arg ~ '=mp6_Undef()' );
        };
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Python::tab($level) ~ 'def f_' ~ $label ~ "(" ~ $meth_args.join(", ") ~ "):";
        push @s, Python::tab($level+1) ~    $invocant.emit_python_name ~ " = [" ~ $invocant.emit_python_name ~ "]";
        for @($args) -> $field { 
            push @s, Python::tab($level+1) ~    $field ~ " = [" ~ $field ~ "]";
        };
        push @s, Python::tab($level+1) ~    "try:";
        push @s,    $block.emit_python_indented($level + 2);
        push @s, Python::tab($level+1) ~    "except mp6_Return, r:";
        push @s, Python::tab($level+2) ~        "return r.value";
        push @s, Python::tab($level) ~ "self.__dict__.update({'f_" ~ $.name ~ "':f_" ~ $label ~ "})";
        return @s.join("\n");
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        if ( $.name eq '' ) {
            # generate an anonymous sub in the current block
            MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                    MiniPerl6::Python::AnonSub.new( 
                        name  => $label, 
                        block => @.block,
                        sig   => $.sig, 
                        handles_return_exception => 1,
                    )
                );
            # return a ref to the anonymous sub
            return Python::tab($level) ~ 'f_' ~ $label;
        }

        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        my $default_args = [];
        my $meth_args = [ 'self' ];
        for @$pos -> $field { 
            my $arg = $field.emit_python_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=mp6_Undef()' );
            $meth_args.push( $arg ~ '=mp6_Undef()' );
        };
        my $block = MiniPerl6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my $label2 = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        my @s;
        push @s, Python::tab($level) ~ "def f_" ~ $.name ~ "(" ~ $default_args.join(", ") ~ "):" 
        for @($args) -> $field { 
            push @s, Python::tab($level+1) ~    $field ~ " = [" ~ $field ~ "]";
        };
        push @s, Python::tab($level+1) ~    "try:";
        push @s,    $block.emit_python_indented($level + 2);
        push @s, Python::tab($level+1) ~    "except mp6_Return, r:";
        push @s, Python::tab($level+2) ~        "return r.value";

        # decorate the sub such that it works as a method
        push @s, Python::tab($level) ~ "global " ~ $label2; 
        push @s, Python::tab($level) ~ $label2 ~ " = f_" ~ $.name;
        push @s, Python::tab($level) ~ "def f_" ~ $label ~ "(" ~ $meth_args.join(", ") ~ "):";
        push @s, Python::tab($level+1) ~    "return " ~ $label2 ~ "(" ~ $args.join(", ") ~ ")";
        push @s, Python::tab($level) ~ "self.__dict__.update({'f_" ~ $.name ~ "':f_" ~ $label ~ "})";
        return @s.join("\n");
    }
}

class Do {
    has @.block;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ MiniPerl6::Python::LexicalBlock::get_ident_python;
        # generate an anonymous sub in the current block
        MiniPerl6::Python::LexicalBlock::push_stmt_python( 
                MiniPerl6::Python::AnonSub.new( 
                    name  => $label, 
                    block => @.block,
                    sig   => Sig.new( invocant => undef, positional => [], named => {} ),
                    handles_return_exception => 0,
                )
            );
        # call the anonymous sub
        return Python::tab($level) ~ "f_" ~ $label ~ "()";
    }
}

class Use {
    has $.mod;
    method emit_python { $self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        # Python::tab($level) ~ 'from ' ~ Main::to_go_namespace($.mod) ~ ' import *'
        return '';
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


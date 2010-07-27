use v6;

class Ruby {
    sub to_str ($op, $args) {
        my @s;
        for @($args) -> $cond {
            if $cond.isa( 'Val::Buf' ) {
                push @s, $cond.emit_ruby;
            }
            else {
                push @s, '(' ~ $cond.emit_ruby ~ ').to_s';
            }
        }
        return '(' ~ @s.join($op) ~ ')'
    }
    sub to_num ($op, $args) {
        my @s;
        for @($args) -> $cond {
            if ($cond.isa( 'Val::Int' )) || ($cond.isa( 'Val::Num' )) {
                push @s, $cond.emit_ruby;
            }
            else {
                push @s, 'mp6_to_num(' ~ $cond.emit_ruby ~ ')'; 
            }
        }
        return '(' ~ @s.join($op) ~ ')'
    }
    sub to_bool ($op, $args) {
        my @s;
        for @($args) -> $cond {
            if ($cond.isa( 'Val::Int' )) 
                || ($cond.isa( 'Val::Num' )) 
                || ($cond.isa( 'Val::Bit' )) 
            {
                push @s, '(' ~ $cond.emit_ruby ~ ' != 0 )';
            }
            elsif  (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<||>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<&&>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<!>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<?>'))
            {
                push @s, $cond.emit_ruby;
            }
            else {
                push @s, 'mp6_to_bool(' ~ $cond.emit_ruby ~ ')'; 
            }
        }
        return '(' ~ @s.join($op) ~ ')'
    }
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

class MiniPerl6::Ruby::AnonSub {
    has $.name;
    has $.sig;
    has @.block;
    has $.handles_return_exception;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        for @$pos -> $field { 
            $args.push( $field.emit_ruby_name );
        };
        my $block = MiniPerl6::Ruby::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Ruby::tab($level)   
            ~ ( $.name ?? ("f_" ~ $.name ~ " = ") !! "" )
            ~ "lambda{ |" ~ $args.join(", ") ~ "| ";

        push @s,    $block.emit_ruby_indented($level + 1); 
        push @s, Ruby::tab($level)   ~ "}";
        return @s.join("\n");

    }
}

class MiniPerl6::Ruby::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;

    my $ident;
    my @anon_block;
    sub push_stmt_ruby($block) { 
        push @anon_block, $block; 
    }
    sub get_ident_ruby {
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

    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
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
                push $block, $decl;
            }
        }
        if @($has_decl) {

            # create accessors
            for @($has_decl) -> $decl {
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    push @s, Ruby::tab($level) ~ 'attr_accessor :v_' ~ ($decl.var).name;
                    push @s, Ruby::tab($level) ~ 'def f_' ~ ($decl.var).name ~ '()';
                    push @s, Ruby::tab($level+1) ~ 'return self.v_' ~ ($decl.var).name;
                    push @s, Ruby::tab($level) ~ "end";
                }
                if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'has' ) {
                    push @s, Ruby::tab($level) ~ 'attr_accessor :v_' ~ (($decl.parameters).var).name;
                    push @s, Ruby::tab($level) ~ 'def f_' ~ (($decl.parameters).var).name ~ '()';
                    push @s, Ruby::tab($level+1) ~ 'return self.v_' ~ (($decl.parameters).var).name;
                    push @s, Ruby::tab($level) ~ "end";
                }
            }

        }

        my $has_my_decl = 0;
        my @my_decl;
        my @my_init;
        my %my_seen;
        for @($block) -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                if !( %my_seen{ ($decl.var).name } ) {
                    push @my_decl, ($decl.var).emit_ruby_name;
                    push @my_init, $decl.emit_ruby_init;
                    $has_my_decl = 1;
                    %my_seen{ ($decl.var).name } = 1;
                }
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                if !( %my_seen{ (($decl.parameters).var).name } ) {
                    push @my_decl, (($decl.parameters).var).emit_ruby_name;
                    push @my_init, ($decl.parameters).emit_ruby_init;
                    $has_my_decl = 1;
                    %my_seen{ (($decl.parameters).var).name } = 1;
                }
            }
        }
        if $has_my_decl {
            push @s, Ruby::tab($level) ~ "Proc.new{ |" ~ @my_decl.join(", ") ~ "|";
        }

        my $last_statement;
        if $.needs_return {
            $last_statement = pop $block;
        }

        for @($block) -> $stmt {
            @anon_block = [];
            my $s2 = $stmt.emit_ruby_indented($level);
            for @anon_block -> $stmt {
                @s.push( $stmt.emit_ruby_indented( $level ) );
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
                    MiniPerl6::Ruby::LexicalBlock.new( block => ($last_statement.body), needs_return => 1 );
                my $otherwise_block = 
                    MiniPerl6::Ruby::LexicalBlock.new( block => ($last_statement.otherwise), needs_return => 1 );

                if $body_block.has_my_decl {
                    $body_block = Return.new( result => Do.new( block => ($last_statement.body) ) );
                }
                if $has_otherwise && $otherwise_block.has_my_decl {
                    $otherwise_block = Return.new( result => Do.new( block => ($last_statement.otherwise) ) );
                }

                $s2 = Ruby::tab($level) ~ 'if ' ~ Ruby::to_bool(' && ', [$cond]) ~ "\n" 
                    ~ $body_block.emit_ruby_indented( $level + 1 );
                if ( $has_otherwise ) {
                    $s2 = $s2 ~ "\n"
                        ~ Ruby::tab($level) ~ "else\n" 
                            ~ $otherwise_block.emit_ruby_indented($level+1)
                        ~ "\n" ~ Ruby::tab($level) ~ "end" 
                }
                else {
                    $s2 = $s2 ~ "\n" ~ Ruby::tab($level) ~ "end" 
                }
            }
            elsif $last_statement.isa( 'Bind' ) {
                $s2 = $last_statement.emit_ruby_indented( $level );
                $s2 = $s2 ~ "\n"
                        ~ Ruby::tab($level) ~ "return " ~ ($last_statement.parameters).emit_ruby;
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                $s2 = $last_statement.emit_ruby_indented( $level );
            }
            else {
                $s2 = Ruby::tab($level) ~ "return " ~ $last_statement.emit_ruby;
            }

            for @anon_block -> $stmt {
                @s.push( $stmt.emit_ruby_indented( $level ) );
            }
            @s.push( $s2 ); 
        }

        if $has_my_decl {
            push @s, Ruby::tab($level) ~ "}.call(" ~ @my_init.join(", ") ~ ")";
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

    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my @s;
        my $block = MiniPerl6::Ruby::LexicalBlock.new( block => @.body );
        my $name = Main::to_go_namespace($.name);

        for @.body -> $decl {
            if $decl.isa('Use') {
                push @s, Ruby::tab($level) ~ "require '" ~ Main::to_go_namespace($decl.mod) ~ ".rb'"
            }
        }

        push @s, Ruby::tab($level)    ~     'class C_' ~ $name;
        push @s, Ruby::tab($level+1)  ~         '$' ~ $name ~ ' = C_' ~ $name ~ '.new()';
        push @s, Ruby::tab($level+1)  ~         'namespace = $' ~ $name;
        push @s,    $block.emit_ruby_indented($level + 1);
        push @s, Ruby::tab($level)    ~     "end";
        return @s.join( "\n" );
    }
}

class Val::Int {
    has $.int;
    method emit_ruby { $.int }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $.int 
    }
}

class Val::Bit {
    has $.bit;
    method emit_ruby { $.bit }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $.bit 
    }
}

class Val::Num {
    has $.num;
    method emit_ruby { $.num }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $.num 
    }
}

class Val::Buf {
    has $.buf;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ '"' ~ Main::javascript_escape_string($.buf) ~ '"' 
    }
}

class Val::Undef {
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 'nil'
    }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 
            $.class.emit_ruby ~ '(' ~ %.fields.emit_ruby ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
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
            my $temp_array = Var.new( 'name' => 'a', 'namespace' => '', 'sigil' => '@', 'twigil' => '' );
            for @.array1 -> $item {
                if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                    || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' )
                {
                    push @block, Call.new(
                                    'method' => 'concat',
                                    'arguments' => [ $item ],
                                    'hyper' => '',
                                    'invocant' => $temp_array
                                );
                }
                else {
                    push @block, Call.new(
                                    'method' => 'push',
                                    'arguments' => [ $item ],
                                    'hyper' => '',
                                    'invocant' => $temp_array
                                );
                }
            }
            push @block, $temp_array;
            my $body_block = MiniPerl6::Ruby::LexicalBlock.new( block => @block );
            return Ruby::tab($level) ~ "Proc.new { |list_a| " ~ "\n"
                ~   $body_block.emit_ruby_indented( $level + 1 ) ~ "\n"
                ~  Ruby::tab($level) ~ "}.call([])";
        }
        else {
            Ruby::tab($level)  
                ~ '[' ~ (@.array1.>>emit_ruby).join(', ') ~ ']';
        }
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $fields = @.hash1;
        my @dict;
        for @$fields -> $field { 
            push @dict, (($field[0]).emit_ruby ~ ' => ' ~ ($field[1]).emit_ruby);
        }; 
        Ruby::tab($level) ~ 
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
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $fields = @.fields;
        my @str;
        for @$fields -> $field { 
            push @str, "o.v_" ~ ($field[0]).buf ~ '=' ~ ($field[1]).emit_ruby ~ "; ";
        }
        Ruby::tab($level) ~ "Proc.new { |o| "
            ~ @str.join(' ')
            ~ "o }.call(C_" ~ Main::to_go_namespace($.class) ~ ".new)";
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 
            $.obj.emit_ruby ~ '[' ~ $.index_exp.emit_ruby ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 
            $.obj.emit_ruby ~ '[' ~ $.index_exp.emit_ruby ~ ']';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.name;
    my $table = {
        '$' => 'v_',
        '@' => 'list_',
        '%' => 'hash_',
        '&' => 'code_',
    };
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        if ($.sigil eq '@') && ($.twigil eq '*') && ($.name eq 'ARGS') {
            return Ruby::tab($level) ~ 'ARGV'
        }
        return Ruby::tab($level) ~ (
               ( $.twigil eq '.' )
            ?? ( 'self.v_' ~ $.name ~ '' )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name ~ '' )
                )
            )
    };
    method emit_ruby_name {
        return (
               ( $.twigil eq '.' )
            ?? ( 'self.v_' ~ $.name )
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
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        if $.parameters.isa( 'Index' ) {
            return Ruby::tab($level)  
                ~ ($.parameters.obj).emit_ruby ~ '['
                    ~ ($.parameters.index_exp).emit_ruby ~ '] = '
                    ~ $.arguments.emit_ruby;
        }
        if $.parameters.isa( 'Lookup' ) {
            return Ruby::tab($level)  
                ~ ($.parameters.obj).emit_ruby ~ '['
                    ~ ($.parameters.index_exp).emit_ruby ~ '] = '
                    ~ $.arguments.emit_ruby;
        }
        if $.parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return Ruby::tab($level)  
                ~ ($.parameters.invocant).emit_ruby ~ ".v_" ~ $.parameters.method ~ " = " ~ $.arguments.emit_ruby ~ "";
        }
        Ruby::tab($level)  
            ~ $.parameters.emit_ruby ~ ' = ' ~ $.arguments.emit_ruby;
    }
}

class Proto {
    has $.name;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ '$' ~ Main::to_go_namespace($.name) 
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $invocant = $.invocant.emit_ruby;
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'isa')
        { 
            if ($.hyper) {
            	return $invocant ~ ".map {|x| x." ~ $.method ~ "(" ~ (@.arguments.>>emit_ruby).join(', ') ~ ")}";
            }
            else {
                return "mp6_" ~ $.method ~ '(' ~ ([ $.invocant, @.arguments].>>emit_ruby).join(', ') ~ ')';
            }
        };

        my $meth = $.method;
        if $meth eq 'postcircumfix:<( )>' {
            return Ruby::tab($level) ~ 
                $invocant ~ '.call(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        if     ( $meth eq 'values' ) 
            || ( $meth eq 'keys' )
            || ( $meth eq 'push' )
            || ( $meth eq 'shift' )
            || ( $meth eq 'concat' )
            || ( $meth eq 'join')
        {
            return Ruby::tab($level) ~ $invocant ~ '.' ~ $meth ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        if $meth eq 'chars' {
            return Ruby::tab($level) ~ "" ~ $invocant ~ ".length";
        }
        
        my $call = 'f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        if ($.hyper) {
            Ruby::tab($level) ~ $invocant ~ ".map {|x| x." ~ $call ~ "}";
        }
        else {
            Ruby::tab($level) ~ $invocant ~ '.' ~ $call;
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 
            $self.emit_ruby
    }
    method emit_ruby {
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_ruby ~ ').(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'self' };
        if $code eq 'make'       { return "v_MATCH.v_capture = " ~ (@.arguments[0]).emit_ruby ~ '' }
        if $code eq 'false'      { return 'false' };
        if $code eq 'true'       { return 'true' };

        if $code eq 'say'        { return 'puts'  ~ Ruby::to_str(' + ', @.arguments) } 
        if $code eq 'print'      { return 'print' ~ Ruby::to_str(' + ', @.arguments) }
        if $code eq 'warn'       { return '$stdout.puts('  ~ (@.arguments.>>emit_ruby).join(', ') ~ ')' }

        if $code eq 'array'      { return '[' ~ (@.arguments.>>emit_ruby).join(' ')      ~ ']' };

        if $code eq 'Int'        { return '(' ~ (@.arguments[0]).emit_ruby     ~ ').to_i' };
        if $code eq 'Num'        { return '(' ~ (@.arguments[0]).emit_ruby     ~ ').to_f' };

        if $code eq 'prefix:<~>' { return Ruby::to_str(' + ', @.arguments) };
        if $code eq 'prefix:<!>' { return '!'   ~ Ruby::to_bool(' && ', @.arguments)       };
        if $code eq 'prefix:<?>' { return '!(!' ~ Ruby::to_bool(' && ', @.arguments) ~ ')' };

        if $code eq 'prefix:<$>' { return 'mp6_to_scalar(' ~ (@.arguments.>>emit_ruby).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_ruby).join(' ')    ~ ')' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_ruby).join(' ')    ~ '}' };

        if $code eq 'infix:<~>'  { return Ruby::to_str(' + ', @.arguments) };
        if $code eq 'infix:<+>'  { return Ruby::to_num(' + ', @.arguments) };
        if $code eq 'infix:<->'  { return Ruby::to_num(' - ', @.arguments) };
        if $code eq 'infix:<*>'  { return Ruby::to_num(' * ', @.arguments) };
        if $code eq 'infix:</>'  { return Ruby::to_num(' / ', @.arguments) };
        
        if $code eq 'infix:<&&>' { return Ruby::to_bool(' && ', @.arguments) }
        if $code eq 'infix:<||>' { return Ruby::to_bool(' || ', @.arguments) } 
        if $code eq 'infix:<eq>' { return Ruby::to_str(' == ', @.arguments) };
        if $code eq 'infix:<ne>' { return Ruby::to_str(' != ', @.arguments) };
 
        if $code eq 'infix:<==>' { return Ruby::to_num(' == ', @.arguments) };
        if $code eq 'infix:<!=>' { return Ruby::to_num(' != ', @.arguments) };
        if $code eq 'infix:<<>'  { return Ruby::to_num(' < ', @.arguments)  };
        if $code eq 'infix:<>>'  { return Ruby::to_num(' > ', @.arguments)  };

        if $code eq 'exists'     {
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_ruby ~ ').has_key?(' ~ ($arg.index_exp).emit_ruby ~ ')';
            }
        }

        if $code eq 'ternary:<?? !!>' { 
            return '(' ~ Ruby::to_bool(' && ', [@.arguments[0]]) ~ ' ? ' 
                    ~ (@.arguments[1]).emit_ruby ~ ' : ' 
                    ~ (@.arguments[2]).emit_ruby ~ ')'
        }
        
        if $code eq 'substr' { 
            return Ruby::to_str(' + ', [@.arguments[0]]) ~ '[' 
                    ~ (@.arguments[1]).emit_ruby ~ ', ' 
                    ~ (@.arguments[2]).emit_ruby 
                ~ ']' 
        } 
        if $code eq 'index' { 
            return '(' ~ (@.arguments[0]).emit_ruby ~ ').index(' ~ (@.arguments[1]).emit_ruby ~ ')' 
        } 
        if $code eq 'shift'   { return (@.arguments[0]).emit_ruby ~ '.shift()' } 
        if $code eq 'pop'     { return (@.arguments[0]).emit_ruby ~ '.pop()'   } 
        if $code eq 'push'    { return (@.arguments[0]).emit_ruby ~ '.push('    ~ (@.arguments[1]).emit_ruby ~ ')' } 
        if $code eq 'unshift' { return (@.arguments[0]).emit_ruby ~ '.unshift(' ~ (@.arguments[1]).emit_ruby ~ ')' } 

        if $.namespace {
            return '$' ~ Main::to_go_namespace($.namespace) ~ '.f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        'namespace.f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
    }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $self.emit_ruby 
    }
}

class Return {
    has $.result;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 'return ' ~ $.result.emit_ruby ~ '';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $has_body = @.body ?? 1 !! 0;
        my $has_otherwise = @.otherwise ?? 1 !! 0;
        my $body_block = MiniPerl6::Ruby::LexicalBlock.new( block => @.body );
        my $otherwise_block = MiniPerl6::Ruby::LexicalBlock.new( block => @.otherwise );

        if $body_block.has_my_decl {
            $body_block = Do.new( block => @.body );
        }
        if $has_otherwise && $otherwise_block.has_my_decl {
            $otherwise_block = Do.new( block => @.otherwise );
        }

        my $s = Ruby::tab($level) ~   'if ' ~ Ruby::to_bool(' && ', [$.cond]) ~ "\n" 
            ~ $body_block.emit_ruby_indented( $level + 1 );
        if ( $has_otherwise ) {
            $s = $s ~ "\n"
                ~ Ruby::tab($level) ~ "else\n" 
                    ~ $otherwise_block.emit_ruby_indented($level+1)
                ~ "\n" ~ Ruby::tab($level) ~ "end" 
        }
        else {
            $s = $s ~ "\n" ~ Ruby::tab($level) ~ "end" 
        }
        return $s;
    }
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $body_block = MiniPerl6::Ruby::LexicalBlock.new( block => @.body );
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
        Ruby::tab($level) ~ 'while ' ~ Ruby::to_bool(' && ', [$.cond]) ~ "\n"
                ~ $body_block.emit_ruby_indented( $level + 1 ) ~ "\n"
        ~ Ruby::tab($level) ~ 'end'
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $body_block = MiniPerl6::Ruby::LexicalBlock.new( block => @.body );
        Ruby::tab($level) ~   'for ' ~ $.topic.emit_ruby_name ~ " in " ~ $.cond.emit_ruby ~ "\n"
                ~ $body_block.emit_ruby_indented( $level + 1 ) ~ "\n"
        ~ Ruby::tab($level) ~   'end'
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.name;
        Ruby::tab($level)
            ~ ( ( $decl eq 'has' )
            ?? ( '' )
            !! $.var.emit_ruby );
    }
    method emit_ruby_init {
        if ($.var).sigil eq '%' {
            return '{}';
        }
        elsif ($.var).sigil eq '@' {
            return '[]';
        }
        else {
            return 'nil';
        }
        return '';
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_ruby {
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
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $args = [];
        my $default_args = [];
        my $meth_args = [];
        for @$pos -> $field { 
            my $arg = $field.emit_ruby_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=nil' );
            $meth_args.push( $arg ~ '=nil' );
        };
        my $block = MiniPerl6::Ruby::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Ruby::tab($level)   ~  'send( :define_method, "f_' ~ $.name ~ '".to_sym, lambda{ |' ~ $default_args.join(", ") ~ '|';
        push @s, Ruby::tab($level+1) ~      $invocant.emit_ruby_name ~ " = self";
        push @s,    $block.emit_ruby_indented($level + 1);
        push @s, Ruby::tab($level)   ~  "} )";
        return @s.join("\n");
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $label = "_anon_" ~ MiniPerl6::Ruby::LexicalBlock::get_ident_ruby;
        if ( $.name eq '' ) {
            # generate an anonymous sub in the current block
            MiniPerl6::Ruby::LexicalBlock::push_stmt_ruby( 
                    MiniPerl6::Ruby::AnonSub.new( 
                        name  => $label, 
                        block => @.block,
                        sig   => $.sig, 
                        handles_return_exception => 1,
                    )
                );
            # return a ref to the anonymous sub
            return Ruby::tab($level) ~ 'f_' ~ $label;
        }

        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        my $default_args = [];
        my $meth_args = [ 'self' ];
        for @$pos -> $field { 
            my $arg = $field.emit_ruby_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=nil' );
            $meth_args.push( $arg ~ '=nil' );
        };
        my $block = MiniPerl6::Ruby::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my $label2 = "_anon_" ~ MiniPerl6::Ruby::LexicalBlock::get_ident_ruby;
        my @s;
        push @s, Ruby::tab($level)   ~  'send( :define_method, "f_' ~ $.name ~ '".to_sym, lambda{ |' ~ $default_args.join(", ") ~ '|';
        push @s,    $block.emit_ruby_indented($level + 1);
        push @s, Ruby::tab($level) ~ "} )";
        return @s.join("\n");
    }
}

class Do {
    has @.block;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my @s;
        push @s, Ruby::tab($level)   ~ "Proc.new{ || ";
        push @s,    (MiniPerl6::Ruby::LexicalBlock.new( block => @.block, needs_return => 0 )).emit_ruby_indented($level+1);
        push @s, Ruby::tab($level)   ~ "}.call()";
        return @s.join("\n");
    }
}

class Use {
    has $.mod;
    method emit_ruby { $self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        # Ruby::tab($level) ~ 'from ' ~ Main::to_go_namespace($.mod) ~ ' import *'
        return '';
    }
}

=begin

=head1 NAME

MiniPerl6::Ruby::Emit - Code generator for MiniPerl6-in-Ruby

=head1 SYNOPSIS

    $program.emit_ruby  # generated Ruby code

=head1 DESCRIPTION

This module generates Ruby code for the MiniPerl6 compiler.

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


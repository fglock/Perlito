use v6;

use Perlito5::AST;

class Ruby {
    sub to_str ($op, $args) {
        my @s;
        for my $cond ( @($args) ) {
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
        for my $cond ( @($args) ) {
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
        for my $cond ( @($args) ) {
            if     ($cond.isa( 'Val::Int' ))
                || ($cond.isa( 'Val::Num' ))
            {
                push @s, '(' ~ $cond.emit_ruby ~ ' != 0 )';
            }
            elsif  (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<||>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'infix:<&&>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<!>'))
                || (($cond.isa( 'Apply' )) && ($cond.code eq 'prefix:<?>'))
                || ($cond.isa( 'Val::Bit' ))
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
        "    " x $level
    }
}

class Perlito5::Ruby::AnonSub {
    has $.name;
    has $.sig;
    has @.block;
    has $.handles_return_exception;
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        for my $field ( @$pos ) {
            $args.push( $field.emit_ruby_name );
        };
        my $block = Perlito5::Ruby::LexicalBlock.new(
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Ruby::tab($level)
            ~ ( $.name ? ("f_" ~ $.name ~ " = ") : "" )
            ~ "lambda\{ |" ~ $args.join(", ") ~ "| ";

        push @s,    $block.emit_ruby_indented($level + 1);
        push @s, Ruby::tab($level)   ~ "}";
        return @s.join("\n");

    }
}

class Perlito5::Ruby::LexicalBlock {
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
        for my $decl ( @.block ) {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                return 1;
            }
            if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>'
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'my'
            {
                return 1;
            }
        }
        return 0;
    }
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        if !(@.block) {
            push @.block, Apply.new( code => 'Mu' );
        }

        my @s;
        my @tmp;
        for my $stmt ( @anon_block ) {
            @tmp.push( $stmt );
        }

        my $has_decl = [];
        my $block = [];
        for my $decl ( @.block ) {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                push $has_decl, $decl;
            }
            if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>'
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'has'
            {
                push $has_decl, $decl;
            }
            else {
                push $block, $decl;
            }
        }
        if @($has_decl) {

            # create accessors
            for my $decl ( @($has_decl) ) {
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    push @s, Ruby::tab($level) ~ 'attr_accessor :v_' ~ ($decl.var).name;
                    push @s, Ruby::tab($level) ~ 'def f_' ~ ($decl.var).name ~ '()';
                    push @s, Ruby::tab($level+1) ~ 'return self.v_' ~ ($decl.var).name;
                    push @s, Ruby::tab($level) ~ "end";
                }
                if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>'
                    && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'has'
                {
                    push @s, Ruby::tab($level) ~ 'attr_accessor :v_' ~ (($decl.arguments[0]).var).name;
                    push @s, Ruby::tab($level) ~ 'def f_' ~ (($decl.arguments[0]).var).name ~ '()';
                    push @s, Ruby::tab($level+1) ~ 'return self.v_' ~ (($decl.arguments[0]).var).name;
                    push @s, Ruby::tab($level) ~ "end";
                }
            }

        }

        my $has_my_decl = 0;
        my @my_decl;
        my @my_init;
        my %my_seen;
        for my $decl ( @($block) ) {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                if !( %my_seen{ ($decl.var).name } ) {
                    push @my_decl, ($decl.var).emit_ruby_name;
                    push @my_init, $decl.emit_ruby_init;
                    $has_my_decl = 1;
                    %my_seen{ ($decl.var).name } = 1;
                }
            }
            if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>'
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'my'
            {
                if !( %my_seen{ (($decl.arguments[0]).var).name } ) {
                    push @my_decl, (($decl.arguments[0]).var).emit_ruby_name;
                    push @my_init, ($decl.arguments[0]).emit_ruby_init;
                    $has_my_decl = 1;
                    %my_seen{ (($decl.arguments[0]).var).name } = 1;
                }
            }
        }
        if $has_my_decl {
            push @s, Ruby::tab($level) ~ "Proc.new\{ |" ~ @my_decl.join(", ") ~ "|";
            $level += 1;
        }

        my $last_statement;
        if $.needs_return {
            $last_statement = pop $block;
        }

        for my $stmt ( @($block) ) {
            @anon_block = ();
            my $s2 = $stmt.emit_ruby_indented($level);
            for my $stmt ( @anon_block ) {
                @s.push( $stmt.emit_ruby_indented( $level ) );
            }
            push @s, $s2;
        }

        if $.needs_return && $last_statement {
            @anon_block = ();
            my $s2;
            if $last_statement.isa( 'If' ) {
                my $cond            = $last_statement.cond;
                my $has_otherwise   = $last_statement.otherwise ? 1 : 0;
                my $body_block      =
                    Perlito5::Ruby::LexicalBlock.new( block => ($last_statement.body.stmts), needs_return => 1 );
                my $otherwise_block =
                    Perlito5::Ruby::LexicalBlock.new( block => ($last_statement.otherwise.stmts), needs_return => 1 );

                if $body_block.has_my_decl() {
                    $body_block = Return.new( result => Do.new( block => ($last_statement.body) ) );
                }
                if $has_otherwise && $otherwise_block.has_my_decl() {
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

            for my $stmt ( @anon_block ) {
                @s.push( $stmt.emit_ruby_indented( $level ) );
            }
            @s.push( $s2 );
        }

        if $has_my_decl {
            $level -= 1;
            push @s, Ruby::tab($level) ~ "}.call(" ~ @my_init.join(", ") ~ ")";
        }

        @anon_block = @tmp;
        return @s.join( "\n" );
    }
}

class CompUnit {
    has %.attributes;
    has %.methods;

    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my @s;
        my $block = Perlito5::Ruby::LexicalBlock.new( block => @.body );
        my $name = Main::to_go_namespace($.name);

        for my $decl ( @.body ) {
            if $decl.isa('Use') {
                push @s, Ruby::tab($level) ~ "require '" ~ Main::to_go_namespace($decl.mod) ~ ".rb'"
                    unless $decl.mod eq 'v6';
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
    method emit_ruby { $.int }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $.int
    }
}

class Val::Bit {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ ( $.bit ? 'true' : 'false' )
    }
}

class Val::Num {
    method emit_ruby { $.num }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ $.num
    }
}

class Val::Buf {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ '"' ~ $.buf ~ '"'
    }
}

class Lit::Block {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        # a block is an anonymous sub
        my $sub = Sub.new( name => '', sig => $.sig, block => @.stmts );
        $sub.emit_ruby_indented( $level );
    }
}

class Lit::Array {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_ruby_indented($level);
    }
}

class Lit::Hash {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_ruby_indented($level);
    }
}

class Index {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~
            $.obj.emit_ruby ~ '[' ~ $.index_exp.emit_ruby ~ ']';
    }
}

class Lookup {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~
            $.obj.emit_ruby ~ '[' ~ $.index_exp.emit_ruby ~ ']';
    }
}

class Var {
    my $table = {
        '$' => 'v_',
        '@' => 'list_',
        '%' => 'hash_',
        '&' => 'code_',
    };
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        if ($.sigil eq '@') && ($.twigil eq '*') && ($.name eq 'ARGS') {
            return Ruby::tab($level) ~ 'ARGV'
        }
        return Ruby::tab($level) ~ (
               ( $.twigil eq '.' )
            ? ( 'self.v_' ~ $.name ~ '' )
            :  (    ( $.name eq '/' )
                ?   ( $table->{$.sigil} ~ 'MATCH' )
                :   ( $table->{$.sigil} ~ $.name ~ '' )
                )
            )
    };
    method emit_ruby_name {
        return (
               ( $.twigil eq '.' )
            ? ( 'self.v_' ~ $.name )
            :  (    ( $.name eq '/' )
                ?   ( $table->{$.sigil} ~ 'MATCH' )
                :   ( $table->{$.sigil} ~ $.name )
                )
            )
    };
}

class Proto {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ '$' ~ Main::to_go_namespace($.name)
    }
}

class Call {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $invocant = $.invocant.emit_ruby;
        if     ($.method eq 'perl')
            || ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'isa')
        {
            if ($.hyper) {
            	return $invocant ~ ".map \{|x| x." ~ $.method ~ "(" ~ (@.arguments.>>emit_ruby).join(', ') ~ ")}";
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
            || ( $meth eq 'unshift' )
            || ( $meth eq 'concat' )
            || ( $meth eq 'join')
        {
            return Ruby::tab($level) ~ $invocant ~ '.' ~ $meth ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        if $meth eq 'chars' {
            return Ruby::tab($level) ~ $invocant ~ ".length";
        }
        if $meth eq 'elems' {
            return Ruby::tab($level) ~ $invocant ~ ".length";
        }

        my $call = 'f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        if ($.hyper) {
            Ruby::tab($level) ~ $invocant ~ ".map \{|x| x." ~ $call ~ "}";
        }
        else {
            Ruby::tab($level) ~ $invocant ~ '.' ~ $call;
        };

    }
}

class Apply {
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~
            self.emit_ruby
    }
    method emit_ruby {
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_ruby ~ ').(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'self' };
        if $code eq 'Mu'         { return 'nil' }

        if $code eq 'make'       { return "v_MATCH.v_capture = " ~ (@.arguments[0]).emit_ruby ~ '' }
        if $code eq 'False'      { return 'false' };
        if $code eq 'True'       { return 'true' };

        if $code eq 'say'        { return 'puts'  ~ Ruby::to_str(' + ', @.arguments) }
        if $code eq 'print'      { return 'print' ~ Ruby::to_str(' + ', @.arguments) }
        if $code eq 'warn'       { return '$stdout.puts('  ~ (@.arguments.>>emit_ruby).join(', ') ~ ')' }
        if $code eq 'return'     { return 'return('  ~ (@.arguments.>>emit_ruby).join(', ') ~ ')' }

        if $code eq 'array'      { return '[' ~ (@.arguments.>>emit_ruby).join(' ')      ~ ']' };

        if $code eq 'Int'        { return '(' ~ (@.arguments[0]).emit_ruby     ~ ').to_i' };
        if $code eq 'Num'        { return '(' ~ (@.arguments[0]).emit_ruby     ~ ').to_f' };

        if $code eq 'prefix:<~>' { return Ruby::to_str(' + ', @.arguments) };
        if $code eq 'prefix:<!>' { return '!'   ~ Ruby::to_bool(' && ', @.arguments)       };
        if $code eq 'prefix:<?>' { return '!(!' ~ Ruby::to_bool(' && ', @.arguments) ~ ')' };

        if $code eq 'prefix:<$>' { return 'mp6_to_scalar(' ~ (@.arguments.>>emit_ruby).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_ruby).join(' ')    ~ ')' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments.>>emit_ruby).join(' ')    ~ '}' };

        if $code eq 'list:<~>'   { return Ruby::to_str(' + ', @.arguments) };
        if $code eq 'infix:<+>'  { return Ruby::to_num(' + ', @.arguments) };
        if $code eq 'infix:<->'  { return Ruby::to_num(' - ', @.arguments) };
        if $code eq 'infix:<*>'  { return Ruby::to_num(' * ', @.arguments) };
        if $code eq 'infix:</>'  { return Ruby::to_num(' / ', @.arguments) };

        if $code eq 'infix:<&&>' { return Ruby::to_bool(' && ', @.arguments) }
        if $code eq 'infix:<||>' { return Ruby::to_bool(' || ', @.arguments) }
        if $code eq 'infix:<and>' { return Ruby::to_bool(' && ', @.arguments) }
        if $code eq 'infix:<or>' { return Ruby::to_bool(' || ', @.arguments) }
        if $code eq 'infix:<eq>' { return Ruby::to_str(' == ', @.arguments) };
        if $code eq 'infix:<ne>' { return Ruby::to_str(' != ', @.arguments) };

        if $code eq 'infix:<==>' { return Ruby::to_num(' == ', @.arguments) };
        if $code eq 'infix:<!=>' { return Ruby::to_num(' != ', @.arguments) };
        if $code eq 'infix:<<>'  { return Ruby::to_num(' < ', @.arguments)  };
        if $code eq 'infix:<>>'  { return Ruby::to_num(' > ', @.arguments)  };

        if $code eq 'infix:<..>' {
            return '(' ~ @.arguments[0].emit_ruby() ~ '..' ~ @.arguments[1].emit_ruby() ~ ')'
        }

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
        if $code eq 'circumfix:<( )>' {
            return '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' {
            return emit_ruby_bind( @.arguments[0], @.arguments[1] );
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
        if $code eq 'defined' { return '(' ~ (@.arguments[0]).emit_ruby ~ ' != nil)' }
        if $code eq 'shift'   { return (@.arguments[0]).emit_ruby ~ '.shift()' }
        if $code eq 'pop'     { return (@.arguments[0]).emit_ruby ~ '.pop()'   }
        if $code eq 'push'    { return (@.arguments[0]).emit_ruby ~ '.push('    ~ (@.arguments[1]).emit_ruby ~ ')' }
        if $code eq 'unshift' { return (@.arguments[0]).emit_ruby ~ '.unshift(' ~ (@.arguments[1]).emit_ruby ~ ')' }
        if $code eq 'elems'   { return (@.arguments[0]).emit_ruby ~ '.length()' }

        if $.namespace {
            return '$' ~ Main::to_go_namespace($.namespace) ~ '.f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
        }
        'namespace.f_' ~ $.code ~ '(' ~ (@.arguments.>>emit_ruby).join(', ') ~ ')';
    }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ self.emit_ruby
    }
    sub emit_ruby_bind($parameters, $arguments) {
        if $parameters.isa( 'Index' ) {
            if      $parameters.obj.isa( 'Var' ) && $parameters.obj.sigil eq '@'
                ||  $parameters.obj.isa( 'Decl' ) && $parameters.obj.var.sigil eq '@'
            {
                return      ($parameters.obj).emit_ruby ~ '['
                    ~       ($parameters.index_exp).emit_ruby ~ '] = '
                    ~       $arguments.emit_ruby
            }
            return    '('
                    ~   $parameters.obj.emit_ruby() ~ ' '
                    ~ '? '
                    ~   ($parameters.obj).emit_ruby ~ '['
                    ~   ($parameters.index_exp).emit_ruby ~ '] = '
                    ~   $arguments.emit_ruby ~ ' '
                    ~ ': Proc.new{|| '
                    ~       ($parameters.obj).emit_ruby ~ ' = [];'
                    ~       ($parameters.obj).emit_ruby ~ '['
                    ~       ($parameters.index_exp).emit_ruby ~ '] = '
                    ~       $arguments.emit_ruby
                    ~   ' }.call() '
                    ~ ')';
        }
        if $parameters.isa( 'Lookup' ) {
            return    ($parameters.obj).emit_ruby ~ '['
                    ~ ($parameters.index_exp).emit_ruby ~ '] = '
                    ~ $arguments.emit_ruby;
        }
        if $parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return ($parameters.invocant).emit_ruby ~ ".v_" ~ $parameters.method ~ " = " ~ $arguments.emit_ruby ~ "";
        }
        if      $parameters.isa( 'Var' ) && $parameters.sigil eq '@'
            ||  $parameters.isa( 'Decl' ) && $parameters.var.sigil eq '@'
        {
            $arguments = Lit::Array.new( array1 => [$arguments] );
        }
        elsif   $parameters.isa( 'Var' ) && $parameters.sigil eq '%'
            ||  $parameters.isa( 'Decl' ) && $parameters.var.sigil eq '%'
        {
            $arguments = Lit::Hash.new( hash1 => [$arguments] );
        }
        return $parameters.emit_ruby ~ ' = ' ~ $arguments.emit_ruby;
    }
}

class Return {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        Ruby::tab($level) ~ 'return ' ~ $.result.emit_ruby ~ '';
    }
}

class If {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $has_body = @.body ? 1 : 0;
        my $has_otherwise = @.otherwise ? 1 : 0;
        my $body_block = Perlito5::Ruby::LexicalBlock.new( block => @.body.stmts );
        if $body_block.has_my_decl() {
            $body_block = Do.new( block => @.body );
        }
        my $s = Ruby::tab($level) ~   'if ' ~ Ruby::to_bool(' && ', [$.cond]) ~ "\n"
            ~ $body_block.emit_ruby_indented( $level + 1 );
        if ( $has_otherwise ) {
            my $otherwise_block = Perlito5::Ruby::LexicalBlock.new( block => @.otherwise.stmts );
            if $otherwise_block.has_my_decl() {
                $otherwise_block = Do.new( block => @.otherwise );
            }
            $s ~= "\n"
                ~ Ruby::tab($level) ~ "else\n"
                ~   $otherwise_block.emit_ruby_indented($level+1)
        }
        $s ~= "\n" ~ Ruby::tab($level) ~ "end";
        return $s;
    }
}

class While {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $body_block = Perlito5::Ruby::LexicalBlock.new( block => @.body );
        if $body_block.has_my_decl() {
            $body_block = Do.new( block => @.body );
        }
        if $.init && $.continue {
            die "not implemented (While)"
            #    'for ( '
            # ~  ( $.init     ? $.init.emit_             ~ '; '  : '; ' )
            # ~  ( $.cond     ? 'f_bool(' ~ $.cond.emit_ ~ '); ' : '; ' )
            # ~  ( $.continue ? $.continue.emit_         ~ ' '   : ' '  )
        }
        Ruby::tab($level) ~ 'while ' ~ Ruby::to_bool(' && ', [$.cond]) ~ "\n"
                ~ $body_block.emit_ruby_indented( $level + 1 ) ~ "\n"
        ~ Ruby::tab($level) ~ 'end'
    }
}

class For {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $body_block = Perlito5::Ruby::LexicalBlock.new( block => @.body.stmts );
        my $topic;
        if $.body.sig() {
            $topic = $.body.sig.emit_ruby_name();
        }
        else {
            $topic = 'v__'
        }
        Ruby::tab($level) ~   'for ' ~ $topic ~ " in " ~ $.cond.emit_ruby ~ "\n"
                ~ $body_block.emit_ruby_indented( $level + 1 ) ~ "\n"
        ~ Ruby::tab($level) ~   'end'
    }
}

class Decl {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $decl = $.decl;
        my $name = $.var.name;
        Ruby::tab($level)
            ~ ( ( $decl eq 'has' )
            ? ( '' )
            : $.var.emit_ruby );
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

class Method {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant;
        my $pos = $sig.positional;
        my $args = [];
        my $default_args = [];
        my $meth_args = [];
        for my $field ( @$pos ) {
            my $arg = $field.emit_ruby_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=nil' );
            $meth_args.push( $arg ~ '=nil' );
        };
        my $block = Perlito5::Ruby::LexicalBlock.new(
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
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $label = "_anon_" ~ Perlito5::Ruby::LexicalBlock::get_ident_ruby;
        if ( $.name eq '' ) {
            # generate an anonymous sub in the current block
            Perlito5::Ruby::LexicalBlock::push_stmt_ruby(
                    Perlito5::Ruby::AnonSub.new(
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
        for my $field ( @$pos ) {
            my $arg = $field.emit_ruby_name;
            $args.push( $arg );
            $default_args.push( $arg ~ '=nil' );
            $meth_args.push( $arg ~ '=nil' );
        };
        my $block = Perlito5::Ruby::LexicalBlock.new(
                block => @.block,
                needs_return => 1 );
        my $label2 = "_anon_" ~ Perlito5::Ruby::LexicalBlock::get_ident_ruby;
        my @s;
        push @s, Ruby::tab($level)   ~  'send( :define_method, "f_' ~ $.name ~ '".to_sym, lambda{ |' ~ $default_args.join(", ") ~ '|';
        push @s,    $block.emit_ruby_indented($level + 1);
        push @s, Ruby::tab($level) ~ "} )";
        return @s.join("\n");
    }
}

class Do {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        my $block = self.simplify.block;
        my @s;
        push @s, Ruby::tab($level)   ~ "Proc.new\{ || ";
        push @s,    (Perlito5::Ruby::LexicalBlock.new( block => $block, needs_return => 0 )).emit_ruby_indented($level+1);
        push @s, Ruby::tab($level)   ~ "}.call()";
        return @s.join("\n");
    }
}

class Use {
    method emit_ruby { self.emit_ruby_indented(0) }
    method emit_ruby_indented( $level ) {
        # Ruby::tab($level) ~ 'from ' ~ Main::to_go_namespace($.mod) ~ ' import *'
        return '';
    }
}

=begin

=head1 NAME

Perlito5::Ruby::Emit - Code generator for Perlito-in-Ruby

=head1 SYNOPSIS

    $program.emit_ruby  # generated Ruby code

=head1 DESCRIPTION

This module generates Ruby code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


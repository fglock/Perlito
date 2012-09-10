use v6;

use Perlito6::AST;

class Python {
    sub tab($level) { 
        "    " x $level
    }

    my %safe_char = (
        '_' => 1,
        ',' => 1,
        '.' => 1,
        ':' => 1,
        '-' => 1,
        '+' => 1,
        '*' => 1,
        ' ' => 1,
        '(' => 1,
        ')' => 1,
        '<' => 1,
        '>' => 1,
        '[' => 1,
        ']' => 1,
    );

    sub escape_string($s) {
        my @out;
        my $tmp = '';
        return "u''" if $s eq '';
        for 0 .. $s.chars() - 1 -> $i {
            my $c = substr($s, $i, 1);
            if     (($c ge 'a') && ($c le 'z'))
                || (($c ge 'A') && ($c le 'Z'))
                || (($c ge '0') && ($c le '9'))
                || exists( %safe_char{$c} )
            {
                $tmp = $tmp ~ $c;
            }
            else {
                @out.push: "u'$tmp'" if $tmp ne '';
                @out.push: "unichr({ ord($c) })";
                $tmp = '';
            }
        }
        @out.push: "u'$tmp'" if $tmp ne '';
        return @out.join(' + ');
    }

}

class Perlito6::Python::AnonSub {
    has $.name;
    has $.sig;
    has @.block;
    has $.handles_return_exception;
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $args = [];
        for @$pos -> $field { 
            $args.push( $field.emit_python_name );
        };
        my $block = Perlito6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Python::tab($level) ~ "def f_" ~ $.name ~ "(" ~ $args.join(", ") ~ "):";
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

class Perlito6::Python::LexicalBlock {
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
            if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' 
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'my' 
            {
                return 1;
            }
        }
        return 0;
    }

    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my @block;
        for @.block {
            if defined($_) {
                push @block, $_
            }
        }

        if !(@block) {
            push @block, Apply.new( code => 'Mu' );
        }

        my @s;
        my @tmp;
        for @anon_block -> $stmt {
            @tmp.push( $stmt );
        }

        my $has_decl = [];
        my $block = [];
        for @block -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                push $has_decl, $decl;
            }
            elsif  $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' 
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'has' 
            {
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
                    my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
                    push @s, Python::tab($level) ~ 'def f_' ~ $label ~ '(v_self):';
                    push @s, Python::tab($level+1) ~ 'return v_self.v_' ~ ($decl.var).name;
                    push @s, Python::tab($level) ~ "self.__dict__.update(\{'f_" ~ $decl.var.name() ~ "':f_" ~ $label ~ "})";
                }
                if     $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' 
                    && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'has' 
                {
                    my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
                    push @s, Python::tab($level) ~ 'def f_' ~ $label ~ '(v_self):';
                    push @s, Python::tab($level+1) ~ 'return v_self.v_' ~ $decl.arguments[0].var.name;
                    push @s, Python::tab($level) ~ "self.__dict__.update(\{'f_" ~ $decl.arguments[0].var.name() ~ "':f_" ~ $label ~ "})";
                }
            }

        }

        for @($block) -> $decl {
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                push @s, Python::tab($level) ~ ($decl.var).emit_python_name() ~ ' = ' ~ $decl.emit_python_init() ~ '';
            }
            elsif  $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' 
                && $decl.arguments[0].isa( 'Decl' ) && $decl.arguments[0].decl eq 'my' 
            {
                push @s, Python::tab($level) ~ $decl.arguments[0].var.emit_python_name() ~ ' = ' ~ $decl.arguments[0].emit_python_init() ~ '';
            }
        }

        my $last_statement;
        if $.needs_return {
            $last_statement = pop @($block);
        }

        for @($block) -> $stmt {
            @anon_block = ();
            my $s2 = $stmt.emit_python_indented($level);
            for @anon_block -> $stmt {
                @s.push( $stmt.emit_python_indented( $level ) );
            }
            push @s, $s2;
        }

        if $.needs_return && $last_statement {
            @anon_block = ();
            my $s2;
            if $last_statement.isa( 'If' ) {
                my $cond            = $last_statement.cond;
                my $has_otherwise   = $last_statement.otherwise ?? 1 !! 0;

                $s2 = Python::tab($level) ~ 'if mp6_to_bool(' ~ $cond.emit_python() ~ "):\n";

                my $body_block = 
                    Perlito6::Python::LexicalBlock.new( block => ($last_statement.body.stmts), needs_return => 1 );
                if $body_block.has_my_decl() {
                    $body_block = Do.new( block => ($last_statement.body) );
                    $s2 = $s2 ~ Python::tab( $level + 1 ) ~ 'return ' ~ $body_block.emit_python();
                }
                else {
                    $s2 = $s2 ~ $body_block.emit_python_indented( $level + 1 );
                }

                if ( $has_otherwise ) {

                    $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "else:\n";

                    my $otherwise_block = 
                        Perlito6::Python::LexicalBlock.new( block => ($last_statement.otherwise.stmts), needs_return => 1 );
                    if $otherwise_block.has_my_decl() {
                        $otherwise_block = Do.new( block => ($last_statement.otherwise) );
                        $s2 = $s2 ~ Python::tab( $level + 1 ) ~ 'return ' ~ $otherwise_block.emit_python();
                    }
                    else {
                        $s2 = $s2 ~ $otherwise_block.emit_python_indented($level+1);
                    }
                }
            }
            elsif $last_statement.isa( 'Apply' ) && $last_statement.code eq 'infix:<=>' {
                $s2 = $last_statement.emit_python_indented( $level );
                $s2 = $s2 ~ "\n"
                        ~ Python::tab($level) ~ "return " ~ $last_statement.arguments[0].emit_python;
            }
            elsif  $last_statement.isa( 'Apply' ) && $last_statement.code eq 'return'
                || $last_statement.isa( 'For' ) 
            {
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
    has %.attributes;
    has %.methods;
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my @s;
        my $block = Perlito6::Python::LexicalBlock.new( block => @.body );
        my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        my $name = Main::to_go_namespace($.name);

        for @.body -> $decl {
            if $decl.isa('Use') {
                if $decl.mod ne 'v6' {
                    push @s, Python::tab($level) ~ 'import ' ~ Main::to_go_namespace($decl.mod)
                }
            }
        }

        push @s, Python::tab($level)    ~   'try:';
        push @s, Python::tab($level+1)  ~       'type(' ~ $name ~ ")";
        push @s, Python::tab($level)    ~   'except NameError:';
        push @s, Python::tab($level+1)  ~       'class ' ~ $name ~ ":";
        push @s, Python::tab($level+2)  ~           "def __init__(self, **arg):";
        push @s, Python::tab($level+3)  ~               "for k in arg:";
        push @s, Python::tab($level+4)  ~                   "self.__dict__[k] = mp6_Scalar()";
        push @s, Python::tab($level+4)  ~                   "self.__dict__[k].f_set(arg[k])";

        push @s, Python::tab($level+2)  ~           "def f__setattr__(self, k, v):";
        push @s, Python::tab($level+3)  ~               "return self.__dict__[k].f_set(v)";
        push @s, Python::tab($level+2)  ~           "def f_isa(self, name):";
        push @s, Python::tab($level+3)  ~               "return name == u'" ~ $.name ~ "'";
        push @s, Python::tab($level+2)  ~           "def f_bool(self):";
        push @s, Python::tab($level+3)  ~               "return 1";

        push @s, Python::tab($level+2)  ~           "def __getattr__(self, attr):";
        push @s, Python::tab($level+3)  ~               "if attr[0:2] == u'v_':";
        push @s, Python::tab($level+4)  ~                   "self.__dict__[attr] = mp6_Scalar()";
        push @s, Python::tab($level+4)  ~                   "return self.__dict__[attr]";
        push @s, Python::tab($level+3)  ~               "raise AttributeError(attr)";

        push @s, Python::tab($level+1)  ~       $name ~ "_proto = " ~ $name ~ "()"; 
        push @s, Python::tab($level+1)  ~       "__builtin__." ~ $name ~ " = " ~ $name ~ ""; 
        push @s, Python::tab($level+1)  ~       "__builtin__." ~ $name ~ "_proto = " ~ $name ~ "_proto"; 

        if $name eq 'GLOBAL' {
            push @s, Python::tab($level)    ~   'self = ' ~ $name;
            push @s, $block.emit_python_indented($level);
        }
        else {
            push @s, Python::tab($level)    ~   'def ' ~ $label ~ "():";
            push @s, Python::tab($level+1)  ~       'self = ' ~ $name;
            push @s,    $block.emit_python_indented($level + 1);
            push @s, Python::tab($level)    ~   $label ~ "()";
        }
        return @s.join( "\n" );
    }
}

class Val::Int {
    method emit_python { $.int }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.int 
    }
}

class Val::Bit {
    method emit_python { $.bit }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.bit 
    }
}

class Val::Num {
    method emit_python { $.num }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ $.num 
    }
}

class Val::Buf {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ Python::escape_string($.buf) 
    }
}

class Lit::Block {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        # generate an anonymous sub in the current block
        my $anon_var = $.sig
            || Var.new( 'name' => '_', 'namespace' => '', 'sigil' => '$', 'twigil' => '' );
        my $anon_sig = Sig.new( invocant => Mu, positional => [ $anon_var ], named => {} );
        Perlito6::Python::LexicalBlock::push_stmt_python( 
                Perlito6::Python::AnonSub.new( 
                    name  => $label, 
                    block => @.stmts,
                    sig   => $anon_sig,
                    handles_return_exception => 0,
                )
            );
        # return a ref to the anonymous sub
        return Python::tab($level) ~ "f_" ~ $label ~ "";
    }
}

class Lit::Array {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_python_indented($level);
    }
}

class Lit::Hash {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_python_indented($level);
    }
}

class Index {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python() ~ '.f_index(' ~ $.index_exp.emit_python() ~ ')';
    }
}

class Lookup {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            $.obj.emit_python() ~ '.f_lookup(' ~ $.index_exp.emit_python() ~ ')';
    }
}

class Var {
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => 'Code_',
    };
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        return Python::tab($level) ~ (
               ( $.twigil eq '.' )
            ?? ( 'v_self.v_' ~ $.name ~ '' )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name ~ '' )
                )
            )
    };
    method emit_python_name {
        return (
               ( $.twigil eq '.' )
            ?? ( 'v_self.v_' ~ $.name )
            !!  (    ( $.name eq '/' )
                ??   ( $table{$.sigil} ~ 'MATCH' )
                !!   ( $table{$.sigil} ~ $.name )
                )
            )
    };
}

class Proto {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        if $.name eq 'self' {
            return Python::tab($level) ~ 'v_self'
        }
        Python::tab($level) ~ 
            Main::to_go_namespace($.name) ~ '_proto'
    }
}

class Call {

    my %method_python = (
        'id'     => 'id',
        'yaml'   => 'yaml',
        'join'   => 'join',
        'split'  => 'split',
        'isa'    => 'isa',
        'say'    => 'say',
    );

    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $invocant = $.invocant.emit_python;

        if $.method eq 'new' {
            my @str;
            for @.arguments -> $field {
                if $field.isa('Apply') && $field.code eq 'infix:<=>>' {
                    @str.push( 'v_' ~ $field.arguments[0].buf() ~ '=' ~ $field.arguments[1].emit_python() );
                }
                else {
                    die 'Error in constructor, field: ', $field.perl;
                }
            }
            return
                Python::tab($level) ~ '__builtin__.' ~ Main::to_go_namespace($.invocant.name) ~ '(' ~ @str.join(', ') ~ ')'
        }

        if exists( %method_python{ $.method } ) {
            if ($.hyper) {
                return Python::tab($level) ~ 'f_map(' ~ $invocant ~ ', lambda x: Main.' ~ $.method ~ '(x, ' ~ (@.arguments>>.emit_python).join(', ') ~ '))';
            }
            else {
                return Python::tab($level) ~ "f_" ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
            }
        }

        my $meth = $.method;
        if $meth eq 'postcircumfix:<( )>' {
            return Python::tab($level) ~ 
                $invocant ~ '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        }
        if     ( $meth eq 'values' ) 
            || ( $meth eq 'keys' )
        {
            return Python::tab($level) ~ $invocant ~ '.' ~ $meth ~ '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        }
        if $meth eq 'chars' {
            return Python::tab($level) ~ "len(" ~ $invocant ~ ")";
        }
        
        my $call = 'f_' ~ $meth ~ '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        if ($.hyper) {
            return Python::tab($level) ~ 'f_map(' ~ $invocant ~ ', lambda x: x.' ~ $call ~ ')';
        }
        else {
            return Python::tab($level) ~ $invocant ~ '.' ~ $call;
        }

    }
}

class Apply {
    method emit_python_indented( $level ) {
        Python::tab($level) ~ 
            self.emit_python
    }
    method emit_python {

        my $apply = self.op_assign();
        if $apply {
            return $apply.emit_python;
        }

        my $code = $.code;
        
        # check that expressions don't overflow the Python parser stack
        if (@.arguments[0]).isa('Apply') {
            my $args2 = @.arguments[0].arguments;
            if ($args2[0]).isa('Apply') 
               && (  $args2[0].code eq 'infix:<or>'
                  || $args2[0].code eq 'infix:<||>' )
            {
                $args2[0] = Do.new( block => $args2[0] );
            }
        }
        if (@.arguments[0]).isa('Apply') {
            my $args2 = @.arguments[0].arguments;
            if ($args2[1]).isa('Apply') 
               && $args2[1].code ne 'infix:<=>>'
            {
                $args2[1] = Do.new( block => $args2[1] );
            }
        }
        if (@.arguments[1]).isa('Apply') {
            my $args2 = @.arguments[1].arguments;
            if ($args2[1]).isa('Apply') 
               && $args2[1].code ne 'infix:<=>>'
            {
                $args2[1] = Do.new( block => $args2[1] );
            }
        }

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_python() ~ ').(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'v_self'   };
        if $code eq 'Mu'         { return 'mp6_Mu()' };
        if $code eq 'make'       { return "v_MATCH.f__setattr__('v_capture', " ~ (@.arguments[0]).emit_python() ~ ')' }
        if $code eq 'False'      { return 'False'       };
        if $code eq 'True'       { return 'True'        };

        if $code eq 'array'      { return '[' ~ (@.arguments>>.emit_python).join(' ')      ~ ']' };

        if $code eq 'Int'        { return 'mp6_to_num(' ~ (@.arguments[0]).emit_python     ~ ')' };
        if $code eq 'Num'        { return 'mp6_to_num(' ~ (@.arguments[0]).emit_python     ~ ')' };

        if $code eq 'prefix:<~>' { return 'unicode('   ~ (@.arguments>>.emit_python).join(' ') ~ ')' };
        if $code eq 'prefix:<!>' { 
            return 'not mp6_to_bool('  ~ (@.arguments>>.emit_python).join(' ') ~ ')' 
        }
        if $code eq 'prefix:<?>' { 
            return 'not (not mp6_to_bool('  ~ (@.arguments>>.emit_python).join(' ')    ~ '))' 
        }

        if $code eq 'prefix:<$>' { return 'mp6_to_scalar(' ~ (@.arguments>>.emit_python).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments>>.emit_python).join(' ')    ~ ')' };
        if $code eq 'prefix:<%>' { return '%{' ~ (@.arguments>>.emit_python).join(' ')    ~ '}' };

        if $code eq 'infix:<x>'  { 
            return     '(unicode(' ~ @.arguments[0].emit_python() ~ ')'
                ~ ' * mp6_to_num(' ~ @.arguments[1].emit_python() ~ '))' 
        };

        if $code eq 'list:<~>'   { return '(unicode('  ~ (@.arguments>>.emit_python).join(') + unicode(')  ~ '))' };
        if $code eq 'infix:<+>'  { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') + mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments>>.emit_python).join(' - ')  ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments>>.emit_python).join(' * ')  ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments>>.emit_python).join(' / ')  ~ ')' };
        
        if   $code eq 'infix:<&&>' 
          || $code eq 'infix:<and>'
        { 
            return 'mp6_and(' ~ (@.arguments[0]).emit_python() ~ ', lambda: ' ~ (@.arguments[1]).emit_python() ~ ')' 
        }
        if   $code eq 'infix:<||>' 
          || $code eq 'infix:<or>'
        { 
            return 'mp6_or('  ~ (@.arguments[0]).emit_python() ~ ', lambda: ' ~ (@.arguments[1]).emit_python() ~ ')' 
        }
        if $code eq 'infix:<//>' { 
            return 'mp6_defined_or('  ~ (@.arguments[0]).emit_python() ~ ', lambda: ' ~ (@.arguments[1]).emit_python() ~ ')' 
        }
        if $code eq 'infix:<eq>' { return '(unicode('  ~ (@.arguments>>.emit_python).join(') == unicode(')  ~ '))' };
        if $code eq 'infix:<ne>' { return '(unicode('  ~ (@.arguments>>.emit_python).join(') != unicode(')  ~ '))' };
        if $code eq 'infix:<ge>' { return '(unicode('  ~ (@.arguments>>.emit_python).join(') >= unicode(')  ~ '))' };
        if $code eq 'infix:<le>' { return '(unicode('  ~ (@.arguments>>.emit_python).join(') <= unicode(')  ~ '))' };

        if $code eq 'infix:<==>' { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') == mp6_to_num(') ~ '))' };
        if $code eq 'infix:<!=>' { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') != mp6_to_num(') ~ '))' };
        if $code eq 'infix:<<>'  { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') < mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<>>'  { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') > mp6_to_num(')  ~ '))' };
        if $code eq 'infix:<<=>' { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') <= mp6_to_num(') ~ '))' };
        if $code eq 'infix:<>=>' { return '(mp6_to_num('  ~ (@.arguments>>.emit_python).join(') >= mp6_to_num(') ~ '))' };
        if $code eq 'infix:<..>' { 
            return 'mp6_Array(range('  ~ (@.arguments[0]).emit_python() ~ ', 1 + ' ~ (@.arguments[1]).emit_python() ~ '))' 
        }
        if $code eq 'infix:<===>' {
             return '(f_id(' ~ (@.arguments[0]).emit_python() ~ ') == f_id(' ~ (@.arguments[1]).emit_python() ~ '))'
        }

        if $code eq 'exists'     {
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_python() ~ ').has_key(' ~ ($arg.index_exp).emit_python() ~ ')';
            }
        }

        if $code eq 'ternary:<?? !!>' { 
            my $ast = 
                Do.new( 
                    block => 
                        If.new(
                            cond      => (@.arguments[0]),
                            body      => Lit::Block.new( stmts => [ @.arguments[1] ] ),
                            otherwise => Lit::Block.new( stmts => [ @.arguments[2] ] ),
                        ),
                );
            return $ast.emit_python;
        }
        if $code eq 'circumfix:<( )>' {
            return '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' {
            return emit_python_bind( @.arguments[0], @.arguments[1] );
        }
        if $code eq 'return' {
            return 'raise mp6_Return(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        }
        
        if $code eq 'substr' { 
            return (@.arguments[0]).emit_python() ~ '[' 
                    ~ 'mp6_to_num(' ~ (@.arguments[1]).emit_python() ~ ')' 
                ~ ':' 
                    ~ ( defined(@.arguments[2])
                      ??     'mp6_to_num(' ~ (@.arguments[1]).emit_python() ~ ') ' 
                         ~ '+ mp6_to_num(' ~ (@.arguments[2]).emit_python() ~ ')'
                      !! ''
                      )
                ~ ']' 
        } 
        if $code eq 'index' { 
            return 'mp6_index(' ~ (@.arguments[0]).emit_python() ~ ', ' ~ (@.arguments[1]).emit_python() ~ ')' 
        } 
        if $code eq 'defined' { return 'not f_isa(' ~ (@.arguments[0]).emit_python() ~ ",'Mu')" } 
        if $code eq 'shift'   { return (@.arguments[0]).emit_python() ~ '.f_shift()' } 
        if $code eq 'pop'     { return (@.arguments[0]).emit_python() ~ '.f_pop()'   } 
        if $code eq 'push'    { return (@.arguments[0]).emit_python() ~ '.f_push('    ~ (@.arguments[1]).emit_python() ~ ')' } 
        if $code eq 'unshift' { return (@.arguments[0]).emit_python() ~ '.f_unshift(' ~ (@.arguments[1]).emit_python() ~ ')' } 

        if $.namespace {
            return Main::to_go_namespace($.namespace) ~ '_proto.f_' ~ $.code ~ '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
        }
        'f_' ~ $.code ~ '(' ~ (@.arguments>>.emit_python).join(', ') ~ ')';
    }
    method emit_python_indented( $level ) {
        Python::tab($level) ~ self.emit_python 
    }
    sub emit_python_bind ($parameters, $arguments) {
        if $parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return   
                ($parameters.invocant).emit_python() ~ ".f__setattr__('v_" ~ $parameters.method() ~ "', " ~ $arguments.emit_python() ~ ")";
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

        return $parameters.emit_python() ~ '.f_set(' ~ $arguments.emit_python() ~ ')';
    }
}

class If {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $has_body = @.body ?? 1 !! 0;
        my $has_otherwise = @.otherwise ?? 1 !! 0;
        my $body_block = Perlito6::Python::LexicalBlock.new( block => @.body.stmts );
        if $body_block.has_my_decl() {
            $body_block = Do.new( block => @.body );
        }
        my $s = Python::tab($level) ~   'if mp6_to_bool(' ~ $.cond.emit_python() ~ "):\n" 
            ~ $body_block.emit_python_indented( $level + 1 );
        if ( $has_otherwise ) {
            my $otherwise_block = Perlito6::Python::LexicalBlock.new( block => @.otherwise.stmts );
            if $otherwise_block.has_my_decl() {
                $otherwise_block = Do.new( block => @.otherwise );
            }
            $s = $s ~ "\n"
                ~ Python::tab($level) ~ "else:\n" 
                    ~ $otherwise_block.emit_python_indented($level+1);
        }
        return $s;
    }
}

class While {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $body_block = Perlito6::Python::LexicalBlock.new( block => @.body.stmts );
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
            ~ 'while mp6_to_bool(' ~ $.cond.emit_python() ~ "):\n"
                ~ $body_block.emit_python_indented( $level + 1 );
    }
}

class For {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $body_block = Perlito6::Python::LexicalBlock.new( block => @.body.stmts );
        my $sig = 'v__';
        if $.body.sig() {
            $sig = $.body.sig.emit_python_name();
        }
        if $body_block.has_my_decl() {
            # wrap the block into a call to anonymous subroutine 
            my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
            # generate an anonymous sub in the current block
            my $anon_var = $.body.sig() 
                || Var.new( 'name' => '_', 'namespace' => '', 'sigil' => '$', 'twigil' => '' );
            my $anon_sig = Sig.new( invocant => Mu, positional => [ $anon_var ], named => {} );
            Perlito6::Python::LexicalBlock::push_stmt_python( 
                    Perlito6::Python::AnonSub.new( 
                        name  => $label, 
                        block => @.body.stmts(),
                        sig   => $anon_sig,
                        handles_return_exception => 0,
                    )
                );
            return Python::tab($level) ~    'for ' ~ $sig ~ " in " ~ $.cond.emit_python() ~ ":\n"
                ~  Python::tab($level+1) ~      "f_" ~ $label ~ "(" ~ $sig ~ ")";
        }
        Python::tab($level) ~   'for ' ~ $sig ~ " in " ~ $.cond.emit_python() ~ ":\n"
                ~ $body_block.emit_python_indented( $level + 1 );
    }
}

class Decl {
    method emit_python { self.emit_python_indented(0) }
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
            return 'mp6_Scalar()';
        }
        return '';
    }
}

class Method {
    method emit_python { self.emit_python_indented(0) }
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
            $default_args.push( $arg ~ '=mp6_Scalar()' );
            $meth_args.push( $arg ~ '=mp6_Scalar()' );
        };
        my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        my $block = Perlito6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my @s;
        push @s, Python::tab($level) ~ 'def f_' ~ $label ~ "(" ~ $meth_args.join(", ") ~ "):";
        push @s, Python::tab($level+1) ~    "try:";
        push @s,    $block.emit_python_indented($level + 2);
        push @s, Python::tab($level+1) ~    "except mp6_Return, r:";
        push @s, Python::tab($level+2) ~        "return r.value";
        push @s, Python::tab($level) ~ "self.__dict__.update(\{'f_" ~ $.name ~ "':f_" ~ $label ~ "})";
        return @s.join("\n");
    }
}

class Sub {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        if ( $.name eq '' ) {
            # generate an anonymous sub in the current block
            Perlito6::Python::LexicalBlock::push_stmt_python( 
                    Perlito6::Python::AnonSub.new( 
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
            $default_args.push( $arg ~ '=mp6_Scalar()' );
            $meth_args.push( $arg ~ '=mp6_Scalar()' );
        }
        my $block = Perlito6::Python::LexicalBlock.new( 
                block => @.block,
                needs_return => 1 );
        my $label2 = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        my @s;
        push @s, Python::tab($level) ~ "def f_" ~ $.name ~ "(" ~ $default_args.join(", ") ~ "):";
        push @s, Python::tab($level+1) ~    "try:";
        push @s,    $block.emit_python_indented($level + 2);
        push @s, Python::tab($level+1) ~    "except mp6_Return, r:";
        push @s, Python::tab($level+2) ~        "return r.value";

        # decorate the sub such that it works as a method
        push @s, Python::tab($level) ~ "global " ~ $label2; 
        push @s, Python::tab($level) ~ $label2 ~ " = f_" ~ $.name;
        push @s, Python::tab($level) ~ "def f_" ~ $label ~ "(" ~ $meth_args.join(", ") ~ "):";
        push @s, Python::tab($level+1) ~    "return " ~ $label2 ~ "(" ~ $args.join(", ") ~ ")";
        push @s, Python::tab($level) ~ "self.__dict__.update(\{'f_" ~ $.name ~ "':f_" ~ $label ~ "})";
        return @s.join("\n");
    }
}

class Do {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        my $label = "_anon_" ~ Perlito6::Python::LexicalBlock::get_ident_python;
        my $block = self.simplify.block;
        # generate an anonymous sub in the current block
        Perlito6::Python::LexicalBlock::push_stmt_python( 
                Perlito6::Python::AnonSub.new( 
                    name  => $label, 
                    block => $block,
                    sig   => Sig.new( invocant => Mu, positional => [], named => {} ),
                    handles_return_exception => 0,
                )
            );
        # call the anonymous sub
        return Python::tab($level) ~ "f_" ~ $label ~ "()";
    }
}

class Use {
    method emit_python { self.emit_python_indented(0) }
    method emit_python_indented( $level ) {
        # Python::tab($level) ~ 'from ' ~ Main::to_go_namespace($.mod) ~ ' import *'
        return '';
    }
}

=begin

=head1 NAME

Perlito6::Python::Emit - Code generator for Perlito-in-Python

=head1 SYNOPSIS

    $program.emit_python  # generated Python code

=head1 DESCRIPTION

This module generates Python code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2010, 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


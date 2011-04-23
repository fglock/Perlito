use v6;

class Javascript {
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

class Perlito::Javascript::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {

        if $.top_level {
            my $block = Perlito::Javascript::LexicalBlock.new( block => self.block, needs_return => self.needs_return, top_level => 0 );
            return  
                  Javascript::tab($level)   ~ 'try {' ~ "\n"
                ~                               $block.emit_javascript_indented( $level + 1 ) ~ ';' ~ "\n"
                ~ Javascript::tab($level)   ~ '}' ~ "\n"
                ~ Javascript::tab($level)   ~ 'catch(err) {' ~ "\n"
                ~ Javascript::tab($level + 1)   ~ 'if ( err instanceof Error ) {' ~ "\n"
                ~ Javascript::tab($level + 2)       ~ 'throw(err);' ~ "\n"
                ~ Javascript::tab($level + 1)   ~ '}' ~ "\n"
                ~ Javascript::tab($level + 1)   ~ 'else {' ~ "\n"
                ~ Javascript::tab($level + 2)       ~ 'return(err);' ~ "\n"
                ~ Javascript::tab($level + 1)   ~ '}' ~ "\n"
                ~ Javascript::tab($level)   ~ '}';
        }
 
        my @block;
        for @.block {
            if defined($_) {
                push @block, $_
            }
        }
        if !@block {
            return Javascript::tab($level) ~ 'null;';
        }
        my @str;
        for @block -> $decl { 
            if $decl.isa( 'Decl' ) && $decl.decl eq 'my' {
                @str.push Javascript::tab($level) ~ $decl.emit_javascript_init; 
            }
            if $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' {
                my $var = $decl.arguments[0];
                if $var.isa( 'Decl' ) && $var.decl eq 'my' {
                    @str.push Javascript::tab($level) ~ $var.emit_javascript_init; 
                }
            }
        }
        my $last_statement;
        if $.needs_return {
            $last_statement = pop @block;
        }
        for @block -> $decl { 
            if !( $decl.isa( 'Decl' ) && $decl.decl eq 'my' ) {
                @str.push $decl.emit_javascript_indented($level) ~ ';';
            }
        }
        if $.needs_return && $last_statement {
            if $last_statement.isa( 'If' ) {
                my $cond      = $last_statement.cond;
                my $body      = $last_statement.body;
                my $otherwise = $last_statement.otherwise;
                if $cond.isa( 'Var' ) && $cond.sigil eq '@' {
                    $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
                }
                $body      = Perlito::Javascript::LexicalBlock.new( block => $body.stmts, needs_return => 1 );
                @str.push Javascript::tab($level) ~ 
                        'if ( f_bool(' ~ $cond.emit_javascript() ~ ') ) { return (function () {' ~ "\n" 
                        ~       $body.emit_javascript_indented($level+1) ~ "\n"
                        ~ Javascript::tab($level) ~ '})(); }';
                if $otherwise { 
                    $otherwise = Perlito::Javascript::LexicalBlock.new( block => $otherwise.stmts, needs_return => 1 );
                    @str.push 
                          Javascript::tab($level) ~ 'else { return (function () {' ~ "\n" 
                        ~       $otherwise.emit_javascript_indented($level+1) ~ "\n"
                        ~ Javascript::tab($level) ~ '})(); }';
                }
            }
            elsif  $last_statement.isa( 'Apply' ) && $last_statement.code eq 'return'
                || $last_statement.isa( 'For' ) 
            {
                # Return, For - no changes for now 
                @str.push $last_statement.emit_javascript_indented($level)
            }
            else {
                @str.push Javascript::tab($level) ~ 'return(' ~ $last_statement.emit_javascript() ~ ')'
            }
        }
        return @str.join("\n") ~ ';';
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $class_name = Main::to_javascript_namespace($.name);
        my $str =
              '// class ' ~ $.name ~ "\n"
            ~ 'if (typeof ' ~ $class_name ~ ' !== \'object\') {' ~ "\n"
            ~ '  ' ~ $class_name ~ ' = function() {};' ~ "\n"
            ~ '  ' ~ $class_name ~ ' = new ' ~ $class_name ~ ';' ~ "\n"
            ~ '  ' ~ $class_name ~ '.f_isa = function (s) { return s == \'' ~ $.name ~ '\'; };' ~ "\n"
            ~ '  ' ~ $class_name ~ '.f_perl = function () { return \'' ~ $.name ~ '.new(\' + Main._dump(this) + \')\'; };' ~ "\n"
            ~ '}' ~ "\n"
            ~ '(function () {' ~ "\n"
            ~ '  var v__NAMESPACE = ' ~ $class_name ~ ';' ~ "\n";

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $str = $str ~ '  ' ~ $decl.emit_javascript_init; 
            }
            if $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' {
                my $var = $decl.arguments[0];
                if $var.isa( 'Decl' ) && $var.decl eq 'my' {
                    $str = $str ~ '  ' ~ $var.emit_javascript_init; 
                }
            }
        }
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  
              ~ '  // accessor ' ~ $decl.var.name() ~ "\n"
              ~ '  ' ~ $class_name ~ '.v_' ~ $decl.var.name() ~ ' = null;' ~ "\n"
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.var.name() 
                    ~ ' = function () { return this.v_' ~ $decl.var.name() ~ '; };' ~ "\n";
            }
            if $decl.isa( 'Method' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $invocant = $sig.invocant;
                my $block    = Perlito::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // method ' ~ $decl.name() ~ "\n"
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name()
                    ~ ' = function (' ~ ($pos.>>emit_javascript).join(', ') ~ ') {' ~ "\n"
              ~ '    var ' ~ $invocant.emit_javascript() ~ ' = this;' ~ "\n"
              ~         $block.emit_javascript_indented( $level + 1 ) ~ "\n"
              ~ '  }' ~ "\n"
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name() ~ ';  // v8 bug workaround' ~ "\n";
            }
            if $decl.isa( 'Sub' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $block    = Perlito::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // sub ' ~ $decl.name() ~ "\n"
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name()
                    ~ ' = function (' ~ ($pos.>>emit_javascript).join(', ') ~ ') {' ~ "\n"
              ~         $block.emit_javascript_indented( $level + 1 ) ~ "\n"
              ~ '  }' ~ "\n";
            }
        } 
        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str = $str ~ ($decl).emit_javascript_indented( $level + 1 ) ~ ';';
            }
        }
        $str = $str ~ '}' 
            ~ ')()' ~ "\n";
    }
    sub emit_javascript_program( $comp_units ) {
        my $str = '';
        for @($comp_units) -> $comp_unit {
            $str = $str ~ $comp_unit.emit_javascript()
        }
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ $.int }
}

class Val::Bit {
    has $.bit;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ ($.bit ?? 'true' !! 'false') }
}

class Val::Num {
    has $.num;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ $.num }
}

class Val::Buf {
    has $.buf;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ '"' ~ Main::javascript_escape_string($.buf) ~ '"' }
}

class Lit::Block {
    has $.sig;
    has @.stmts;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        return (@.stmts.>>emit_javascript_indented($level)).join(";\n") ~ ';'
            if @.stmts.elems;
        return '';
    }
}

class Lit::Array {
    has @.array1;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_javascript_indented( $level );
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_javascript_indented( $level );
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ $.obj.emit_javascript() ~ '[' ~ $.index_exp.emit_javascript() ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        # my $var = $.obj.emit_javascript;
        # return $var ~ '[' ~ $.index_exp.emit_javascript() ~ ']'

        my $str = '';
        my $var = $.obj;
        my $var_js;
        if $var.isa('Lookup') {
            my $var1 = $var.obj;
            my $var1_js = $var1.emit_javascript;
            $str = $str ~ 'if (' ~ $var1_js ~ ' == null) { ' ~ $var1_js ~ ' = {} }; ';
            $var_js = $var1_js ~ '[' ~ $var.index_exp.emit_javascript() ~ ']'
        }
        else {
            $var_js = $var.emit_javascript;
        }
        $str = $str ~ 'if (' ~ $var_js ~ ' == null) { ' ~ $var_js ~ ' = {} }; ';
        my $index_js = $.index_exp.emit_javascript;
        $str = $str ~ 'return (' ~ $var_js ~ '[' ~ $index_js ~ '] ' ~ '); ';
        return Javascript::tab($level) ~ '(function () { ' ~ $str ~ '})()';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $table = {
            '$' => 'v_',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        }
        my $ns = '';
        if $.namespace {
            $ns = Main::to_javascript_namespace($.namespace) ~ '.';
        }
           ( $.twigil eq '.' )
        ?? ( 'v_self.v_' ~ $.name ~ '' )
        !!  (    ( $.name eq '/' )
            ??   ( $table{$.sigil} ~ 'MATCH' )
            !!   ( $table{$.sigil} ~ $ns ~ $.name )
            )
    }
    method plain_name {
        if $.namespace {
            return $.namespace ~ '.' ~ $.name
        }
        return $.name
    }
}

class Proto {
    has $.name;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ Main::to_javascript_namespace($.name)        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $invocant = $.invocant.emit_javascript;
        if $invocant eq 'self' {
            $invocant = 'v_self';
        }

        if $.method eq 'new' {
            my $str = [];
            for @.arguments -> $field { 
                if $field.isa('Apply') && $field.code eq 'infix:<=>>' {
                    $str.push( 'v_' ~ $field.arguments[0].buf() ~ ': ' ~ $field.arguments[1].emit_javascript() );
                }
                else {
                    die 'Error in constructor, field: ', $field.perl;
                }
            }
            return
                  '(function () { ' 
                ~   'var tmp = {' ~ $str.join(',') ~ '}; '
                ~   'tmp.__proto__ = ' ~ Main::to_javascript_namespace($invocant) ~ '; '
                ~   'return tmp; '
                ~ '})()'
        }

        if     ($.method eq 'perl')
            || ($.method eq 'isa')
            || ($.method eq 'id')
            || ($.method eq 'scalar')
            || ($.method eq 'keys')
            || ($.method eq 'values')
            || ($.method eq 'pairs')
            || ($.method eq 'elems')
            || ($.method eq 'say' )
            || ($.method eq 'chars')
        { 
            if ($.hyper) {
                return 
                    '(function (a_) { '
                        ~ 'var out = []; ' 
                        ~ 'if ( a_ == null ) { return out; }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( f_' ~ $.method ~ '(a_[i]) ) } return out;'
                    ~ ' })(' ~ $invocant ~ ')'
            }
            return 'f_' ~ $.method ~ '(' 
                    ~ $invocant 
                    ~ ( @.arguments ?? ', ' ~ (@.arguments.>>emit_javascript).join(', ') !! '' ) 
                ~ ')';
        }
        if    ($.method eq 'join') 
           || ($.method eq 'shift') 
           || ($.method eq 'unshift') 
           || ($.method eq 'push') 
           || ($.method eq 'pop') 
        {
            return $invocant ~ '.' ~ $.method ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }

        my $meth = $.method;
        if ($.hyper) {
            return
                    '(function (a_) { '
                        ~ 'var out = []; ' 
                        ~ 'if ( a_ == null ) { return out; }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( a_[i].f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ') ) '
                        ~ '}; '
                        ~ 'return out;'
                    ~ ' })(' ~ $invocant ~ ')'
        }
        if  $meth eq 'postcircumfix:<( )>'  {
            return '(' ~ $invocant ~ ')(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }
        return Javascript::tab($level) ~ $invocant ~ '.f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return Javascript::tab($level) ~ '(' ~ $.code.emit_javascript() ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        }

        if $code eq 'self'       { return Javascript::tab($level) ~ 'v_self' }
        if $code eq 'Mu'         { return Javascript::tab($level) ~ 'null' }
        if $code eq 'make'       { return Javascript::tab($level) ~ '(v_MATCH.v_capture = ' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')' }
        if $code eq 'defined'    { return Javascript::tab($level) ~ '('  ~ (@.arguments.>>emit_javascript).join(' ')    ~ ' != null)' }
        if $code eq 'substr' { 
            return '(' ~ (@.arguments[0]).emit_javascript() ~
                 ' || "").substr(' ~ (@.arguments[1]).emit_javascript() ~
                 ', ' ~ (@.arguments[2]).emit_javascript() ~ ')' 
        }
        if $code eq 'Int'        { return 'parseInt(' ~ (@.arguments[0]).emit_javascript() ~ ')' }
        if $code eq 'Num'        { return 'parseFloat(' ~ (@.arguments[0]).emit_javascript() ~ ')' }
        if $code eq 'prefix:<~>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ').f_string()' }
        if $code eq 'prefix:<!>' { return '( f_bool(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? false : true)' }
        if $code eq 'prefix:<?>' { return '( f_bool(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? true : false)' }
        if $code eq 'prefix:<$>' { return 'f_scalar(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' }
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' };  # .f_array()' }
        if $code eq 'prefix:<%>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ').f_hash()' }

        if $code eq 'postfix:<++>' { return '('   ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return '('   ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')--' }
        if $code eq 'prefix:<++>'  { return '++(' ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')' }
        if $code eq 'prefix:<-->'  { return '--(' ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')' }

        if $code eq 'list:<~>'   { return '(f_string(' ~ (@.arguments.>>emit_javascript()).join(  ') + f_string('  ) ~ '))' }
        if $code eq 'infix:<+>'  { return 'f_add('  ~ (@.arguments.>>emit_javascript).join(', ')  ~ ')' }
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_javascript).join(' - ')   ~ ')' }
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' * ')   ~ ')' }
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' / ')   ~ ')' }
        if $code eq 'infix:<>>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' > ')   ~ ')' }
        if $code eq 'infix:<<>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' < ')   ~ ')' }
        if $code eq 'infix:<>=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' >= ')  ~ ')' }
        if $code eq 'infix:<<=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' <= ')  ~ ')' }
        if $code eq 'infix:<=>>' { return '('  ~ (@.arguments.>>emit_javascript).join(', ')  ~ ')' }

        if $code eq 'infix:<..>' { 
            return '(function (a) { '  
                    ~ 'for (var i=' ~ @.arguments[0].emit_javascript() 
                           ~ ', l=' ~ @.arguments[1].emit_javascript() ~ '; '
                       ~ 'i<=l; ++i)'
                    ~ '{ '
                        ~ 'a.push(i) '
                    ~ '}; '
                    ~ 'return a '  
                ~ '})([])' 
        }
    
        if   $code eq 'infix:<&&>' 
          || $code eq 'infix:<and>'
        { 
            return 'f_and('
                ~ @.arguments[0].emit_javascript() ~ ', ' 
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })' 
        }
        if   $code eq 'infix:<||>'    
          || $code eq 'infix:<or>'
        { 
            return 'f_or('  
                ~ @.arguments[0].emit_javascript() ~ ', ' 
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })' 
        }
        if $code eq 'infix:<//>' { return 'f_defined_or('  
                ~ @.arguments[0].emit_javascript() ~ ', ' 
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })' 
        }
        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit_javascript).join(' == ')  ~ ')' }
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit_javascript).join(' != ')  ~ ')' }
        if $code eq 'infix:<ge>' { return '('  ~ (@.arguments.>>emit_javascript).join(' >= ')  ~ ')' }
        if $code eq 'infix:<le>' { return '('  ~ (@.arguments.>>emit_javascript).join(' <= ')  ~ ')' }
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit_javascript).join(' == ')  ~ ')' }
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' != ')  ~ ')' }
        if $code eq 'infix:<===>' {
            return '(f_id(' ~ (@.arguments[0]).emit_javascript() ~ ') == f_id(' ~ (@.arguments[1]).emit_javascript() ~ '))'
        }

        if $code eq 'exists'     { 
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_javascript() ~ ').hasOwnProperty(' ~ ($arg.index_exp).emit_javascript() ~ ')';
            }
        }
        if $code eq 'ternary:<?? !!>' { 
            return '( f_bool(' ~ (@.arguments[0]).emit_javascript() ~ ')' 
                 ~ ' ? ' ~ (@.arguments[1]).emit_javascript() 
                 ~ ' : ' ~ (@.arguments[2]).emit_javascript() 
                 ~ ')' 
        }
        if $code eq 'circumfix:<( )>' {
            return '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }
        if $code eq 'infix:<=>' {
            return emit_javascript_bind( @.arguments[0], @.arguments[1] );
        }
        if $code eq 'return' {
            return Javascript::tab($level) ~ 'throw(' 
                ~   (@.arguments ?? @.arguments[0].emit_javascript() !! 'null')
                ~ ')'
        }

        $code = 'f_' ~ $.code;
        if $.namespace {
            $code = Main::to_javascript_namespace($.namespace) ~ '.' ~ $code;
        }
        elsif  ($code ne 'f_index') 
            && ($code ne 'f_die') 
            && ($code ne 'f_shift') 
            && ($code ne 'f_unshift') 
            && ($code ne 'f_push') 
            && ($code ne 'f_pop') 
            && ($code ne 'f_chr') 
            && ($code ne 'f_say') 
            && ($code ne 'f_print') 
            && ($code ne 'f_warn') 
        {
            $code = 'v__NAMESPACE.' ~ $code;
        }
        Javascript::tab($level) ~ $code ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }

    # TODO - this would be called from emit_javascript_bind()
    # sub emit_javascript_autovivify ($var) {
    #     if $var.isa('Lookup') {
    #         my $var1 = $var.obj;
    #         my $var1_js = $var1.emit_javascript;
    #         $str = $str ~ 'if (' ~ $var1_js ~ ' == null) { ' ~ $var1_js ~ ' = {} }; ';
    #         $var_js = $var1_js ~ '[' ~ $var.index_exp.emit_javascript() ~ ']'
    #     }
    #     elsif $var.isa('Index') {
    #         my $var1 = $var.obj;
    #         my $var1_js = $var1.emit_javascript;
    #         $str = $str ~ 'if (' ~ $var1_js ~ ' == null) { ' ~ $var1_js ~ ' = [] }; ';
    #         $var_js = $var1_js ~ '[' ~ $var.index_exp.emit_javascript() ~ ']'
    #     }
    #     else {
    #         $var_js = $var.emit_javascript;
    #     }
    # }

    sub emit_javascript_bind ($parameters, $arguments) {
        if $parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $parameters.array1;
            my $str = 'do { ';
            my $i = 0;
            for @$a -> $var { 
                $str = $str ~ ' ' 
                    ~ emit_javascript_bind($var, Index.new(
                        obj    => $arguments,
                        index_exp  => Val::Int.new( int => $i )
                    )) 
                    ~ '; ';
                $i = $i + 1;
            }
            return $str ~ $parameters.emit_javascript() ~ ' }';
        }
        if $parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} = { a => 1, b => [2, 3]}

            my $a = $parameters.hash1;
            my $b = $arguments.hash1;
            my $str = 'do { ';
            my $i = 0;
            my $arg;
            for @$a -> $var {
                $arg = Apply.new(code => 'Mu');
                for @$b -> $var2 {
                    if ($var2[0]).buf eq ($var[0]).buf() {
                        $arg = $var2[1];
                    }
                }
                $str = $str ~ ' ' ~ emit_javascript_bind($var[1], $arg) ~ '; ';
                $i = $i + 1;
            }
            return $str ~ $parameters.emit_javascript() ~ ' }';
        }
        if $parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return '(' ~ ($parameters.invocant).emit_javascript() ~ '.v_' ~ $parameters.method() ~ ' = ' ~ $arguments.emit_javascript() ~ ')';
        }
        if $parameters.isa( 'Lookup' ) {
            my $str = '';
            my $var = $parameters.obj;
            my $var_js;
            if $var.isa('Lookup') {
                my $var1 = $var.obj;
                my $var1_js = $var1.emit_javascript;
                $str = $str ~ 'if (' ~ $var1_js ~ ' == null) { ' ~ $var1_js ~ ' = {} }; ';
                $var_js = $var1_js ~ '[' ~ $var.index_exp.emit_javascript() ~ ']'
            }
            else {
                $var_js = $var.emit_javascript;
            }
            $str = $str ~ 'if (' ~ $var_js ~ ' == null) { ' ~ $var_js ~ ' = {} }; ';
            my $index_js = $parameters.index_exp.emit_javascript;
            $str = $str ~ 'return (' ~ $var_js ~ '[' ~ $index_js ~ '] ' ~ ' = ' ~ $arguments.emit_javascript() ~ '); ';
            return '(function () { ' ~ $str ~ '})()';
        }
        if $parameters.isa( 'Index' ) {
            my $str = '';
            my $var = $parameters.obj;
            my $var_js;
            if $var.isa('Index') {
                my $var1 = $var.obj;
                my $var1_js = $var1.emit_javascript;
                $str = $str ~ 'if (' ~ $var1_js ~ ' == null) { ' ~ $var1_js ~ ' = [] }; ';
                $var_js = $var1_js ~ '[' ~ $var.index_exp.emit_javascript() ~ ']'
            }
            else {
                $var_js = $var.emit_javascript;
            }
            $str = $str ~ 'if (' ~ $var_js ~ ' == null) { ' ~ $var_js ~ ' = [] }; ';
            my $index_js = $parameters.index_exp.emit_javascript;
            $str = $str ~ 'return (' ~ $var_js ~ '[' ~ $index_js ~ '] ' ~ ' = ' ~ $arguments.emit_javascript() ~ '); ';
            return '(function () { ' ~ $str ~ '})()';
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
        '(' ~ $parameters.emit_javascript() ~ ' = ' ~ $arguments.emit_javascript() ~ ')';
    }
}

class If {
    has $.cond;
    has $.body;
    has $.otherwise;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $cond = $.cond;
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
        my $body      = Perlito::Javascript::LexicalBlock.new( block => $.body.stmts, needs_return => 0 );
        my $s = Javascript::tab($level) ~ 'if ( f_bool(' ~ $cond.emit_javascript() ~ ') ) { ' 
            ~ '(function () {' ~ "\n"
            ~       $body.emit_javascript_indented( $level + 1 ) ~ "\n"
            ~ Javascript::tab($level) ~ '})(); }';
        if $.otherwise { 
            my $otherwise = Perlito::Javascript::LexicalBlock.new( block => $.otherwise.stmts, needs_return => 0 );
            $s = $s 
                ~ "\n"
                ~ Javascript::tab($level) ~ 'else { ' 
                ~   '(function () {' ~ "\n"
                ~       $otherwise.emit_javascript_indented( $level + 1 ) ~ "\n"
                ~ Javascript::tab($level) ~ '})(); }';
        }
        return $s;
    }
}


class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $body      = Perlito::Javascript::LexicalBlock.new( block => @.body.stmts, needs_return => 0 );
        return
           Javascript::tab($level) ~ 'for ( '
        ~  ( $.init     ?? $.init.emit_javascript()             ~ '; '  !! '; ' )
        ~  ( $.cond     ?? 'f_bool(' ~ $.cond.emit_javascript() ~ '); ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_javascript()         ~ ' '   !! ' '  )
        ~  ') { '
            ~ '(function () { ' ~ $body.emit_javascript_indented( $level + 1 )      ~ ' })()' 
        ~ ' }'
    }
}

class For {
    has $.cond;
    has @.body;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $cond = $.cond;
        if !( $cond.isa( 'Var' ) && $cond.sigil eq '@' ) {
            $cond = Lit::Array.new( array1 => [$cond] )
        }
        my $body      = Perlito::Javascript::LexicalBlock.new( block => @.body.stmts, needs_return => 0 );
        my $sig = 'v__';
        if $.body.sig() {
            $sig = $.body.sig.emit_javascript_indented( $level + 1 );
        }
        Javascript::tab($level) ~ '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { ' 
            ~ "(function ($sig) \{ "
                ~ $body.emit_javascript_indented( $level + 1 )
            ~ ' })(a_[i_]) } })' 
        ~ '(' ~ $cond.emit_javascript() ~ ')'
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ $.var.emit_javascript;
    }
    method emit_javascript_init {
        if $.decl eq 'my' {
            my $str = "";
            $str = $str ~ 'var ' ~ ($.var).emit_javascript() ~ ' = ';
            if ($.var).sigil eq '%' {
                $str = $str ~ '{};' ~ "\n";
            }
            elsif ($.var).sigil eq '@' {
                $str = $str ~ '[];' ~ "\n";
            }
            else {
                $str = $str ~ 'null;' ~ "\n";
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
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = $pos.>>emit_javascript.join(', ');  
          Javascript::tab($level) ~ 'function ' ~ $.name ~ '(' ~ $str ~ ') {' ~ "\n"
        ~   (Perlito::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
        ~ Javascript::tab($level) ~ '}'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = $pos.>>emit_javascript.join(', ');  
          Javascript::tab($level) ~ 'function ' ~ $.name ~ '(' ~ $str ~ ') {' ~ "\n"
        ~   (Perlito::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
        ~ Javascript::tab($level) ~ '}'
    }
}

class Do {
    has @.block;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $block = self.simplify.block;
        return
              Javascript::tab($level) ~ '(function () { ' ~ "\n"
            ~   (Perlito::Javascript::LexicalBlock.new( block => $block, needs_return => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
            ~ Javascript::tab($level) ~ '})()'
    }
}

class Use {
    has $.mod;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ '// use ' ~ $.mod ~ "\n"
    }
}

=begin

=head1 NAME 

Perlito::Javascript::Emit - Code generator for Perlito-in-Javascript

=head1 SYNOPSIS

    $program.emit_javascript()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Javascript code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

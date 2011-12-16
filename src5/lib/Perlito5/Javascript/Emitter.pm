use v6;

use Perlito5::AST;

class Javascript {
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
        return "''" if $s eq '';
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
                @out.push: "'$tmp'" if $tmp ne '';
                @out.push: "String.fromCharCode({ ord($c) })";
                $tmp = '';
            }
        }
        @out.push: "'$tmp'" if $tmp ne '';
        return @out.join(' + ');
    }

    my %reserved = (
        print => 1,
    );

    sub escape_function ($s) {
        return 'f_' ~ $s if exists %reserved{$s};
        return $s;
    }

}

class Perlito5::Javascript::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {

        if $.top_level {
            my $block = Perlito5::Javascript::LexicalBlock.new( block => self.block, needs_return => self.needs_return, top_level => 0 );
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
                @str.push: Javascript::tab($level) ~ $decl.emit_javascript_init;
            }
            if $decl.isa( 'Apply' ) && $decl.code eq 'infix:<=>' {
                my $var = $decl.arguments[0];
                if $var.isa( 'Decl' ) && $var.decl eq 'my' {
                    @str.push: Javascript::tab($level) ~ $var.emit_javascript_init;
                }
            }
        }
        my $last_statement;
        if $.needs_return {
            $last_statement = pop @block;
        }
        for @block -> $decl {
            if !( $decl.isa( 'Decl' ) && $decl.decl eq 'my' ) {
                @str.push: $decl.emit_javascript_indented($level) ~ ';';
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
                $body      = Perlito5::Javascript::LexicalBlock.new( block => $body.stmts, needs_return => 1 );
                @str.push: Javascript::tab($level) ~
                        'if ( ' ~ Javascript::escape_function('bool') ~ '(' ~ $cond.emit_javascript() ~ ') ) { return (function () {' ~ "\n"
                        ~       $body.emit_javascript_indented($level+1) ~ "\n"
                        ~ Javascript::tab($level) ~ '})(); }';
                if $otherwise {
                    $otherwise = Perlito5::Javascript::LexicalBlock.new( block => $otherwise.stmts, needs_return => 1 );
                    @str.push:
                          Javascript::tab($level) ~ 'else { return (function () {' ~ "\n"
                        ~       $otherwise.emit_javascript_indented($level+1) ~ "\n"
                        ~ Javascript::tab($level) ~ '})(); }';
                }
            }
            elsif  $last_statement.isa( 'Apply' ) && $last_statement.code eq 'return'
                || $last_statement.isa( 'For' )
            {
                # Return, For - no changes for now
                @str.push: $last_statement.emit_javascript_indented($level)
            }
            else {
                @str.push: Javascript::tab($level) ~ 'return(' ~ $last_statement.emit_javascript() ~ ')'
            }
        }
        return @str.join("\n") ~ ';';
    }
}

class CompUnit {
    has %.attributes;
    has %.methods;
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $class_name = Main::to_javascript_namespace($.name);
        my $str =
              '// class ' ~ $.name ~ "\n"
            ~ 'if (typeof ' ~ $class_name ~ ' !== \'object\') {' ~ "\n"
            ~ '  ' ~ $class_name ~ ' = function() {};' ~ "\n"
            ~ '  ' ~ $class_name ~ ' = new ' ~ $class_name ~ ';' ~ "\n"
            ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function('isa') ~ ' = function (s) { return s == \'' ~ $.name ~ '\'; };' ~ "\n"
            ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function('perl') ~ ' = function () { return \'' ~ $.name ~ '.new(\' + Main._dump(this) + \')\'; };' ~ "\n"
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
              ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function( $decl.var.name() )
                    ~ ' = function () { return this.v_' ~ $decl.var.name() ~ '; };' ~ "\n";
            }
            if $decl.isa( 'Method' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $invocant = $sig.invocant;
                my $block    = Perlito5::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str
              ~ '  // method ' ~ $decl.name() ~ "\n"
              ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function( $decl.name() )
                    ~ ' = function (' ~ ($pos.>>emit_javascript).join(', ') ~ ') {' ~ "\n"
              ~ '    var ' ~ $invocant.emit_javascript() ~ ' = this;' ~ "\n"
              ~         $block.emit_javascript_indented( $level + 1 ) ~ "\n"
              ~ '  }' ~ "\n"
              ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function( $decl.name() ) ~ ';  // v8 bug workaround' ~ "\n";
            }
            if $decl.isa( 'Sub' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $block    = Perlito5::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str
              ~ '  // sub ' ~ $decl.name() ~ "\n"
              ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function( $decl.name() )
                    ~ ' = function (' ~ ($pos.>>emit_javascript).join(', ') ~ ') {' ~ "\n"
              ~         $block.emit_javascript_indented( $level + 1 ) ~ "\n"
              ~ '  }' ~ "\n"
              ~ '  ' ~ $class_name ~ '.' ~ Javascript::escape_function( $decl.name() ) ~ ';  // v8 bug workaround' ~ "\n";
            }
        }
        for @.body -> $decl {
            if    defined( $decl )
               && (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
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
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ $.int }
}

class Val::Bit {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ ($.bit ?? 'true' !! 'false') }
}

class Val::Num {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ $.num }
}

class Val::Buf {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) { Javascript::tab($level) ~ Javascript::escape_string($.buf) }
}

class Lit::Block {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $sig = 'v__';
        if $.sig {
            $sig = $.sig.emit_javascript_indented( $level + 1 );
        }
        return
              Javascript::tab($level) ~ "(function ($sig) \{\n"
            ~   (Perlito5::Javascript::LexicalBlock.new( block => @.stmts, needs_return => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
            ~ Javascript::tab($level) ~ '})'
    }
}

class Lit::Array {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_javascript_indented( $level );
    }
}

class Lit::Hash {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $ast = self.expand_interpolation;
        return $ast.emit_javascript_indented( $level );
    }
}

class Index {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ $.obj.emit_javascript() ~ '[' ~ $.index_exp.emit_javascript() ~ ']';
    }
}

class Lookup {
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
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ Main::to_javascript_namespace($.name)
    }
}

class Call {

    my %method_js = (
        'perl'   => 'perl',
        'isa'    => 'isa',
        'id'     => 'id',
        'scalar' => 'scalar',
        'keys'   => 'keys',
        'values' => 'values',
        'pairs'  => 'pairs',
        'elems'  => 'elems',
        'say'    => 'say',
        'chars'  => 'chars',
    );
    my %method_native_js = (
        'join'    => 'join',
        'split'   => 'split',
        'shift'   => 'shift',
        'unshift' => 'unshift',
        'push'    => 'push',
        'pop'     => 'pop',
    );

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

        if exists( %method_js{ $.method } ) {
            if ($.hyper) {
                return
                    '(function (a_) { '
                        ~ 'var out = []; '
                        ~ 'if ( a_ == null ) { return out; }; '
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( ' ~ Javascript::escape_function( $.method ) ~ '(a_[i]) ) } return out;'
                    ~ ' })(' ~ $invocant ~ ')'
            }
            return Javascript::escape_function( $.method ) ~ '('
                    ~ $invocant
                    ~ ( @.arguments ?? ', ' ~ (@.arguments.>>emit_javascript).join(', ') !! '' )
                ~ ')';
        }

        if exists( %method_native_js{ $.method } ) {
            return $invocant ~ '.' ~ $.method ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }

        my $meth = $.method;
        if ($.hyper) {
            return
                    '(function (a_) { '
                        ~ 'var out = []; '
                        ~ 'if ( a_ == null ) { return out; }; '
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( a_[i].' ~ Javascript::escape_function( $meth ) ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ') ) '
                        ~ '}; '
                        ~ 'return out;'
                    ~ ' })(' ~ $invocant ~ ')'
        }
        if  $meth eq 'postcircumfix:<( )>'  {
            return '(' ~ $invocant ~ ')(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }
        return Javascript::tab($level) ~ $invocant ~ '.' ~ Javascript::escape_function( $meth ) ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }
}

class Apply {

    my %op_infix_js = (
        'infix:<->'  => ' - ',
        'infix:<*>'  => ' * ',
        'infix:</>'  => ' / ',
        'infix:<>>'  => ' > ',
        'infix:<<>'  => ' < ',
        'infix:<>=>' => ' >= ',
        'infix:<<=>' => ' <= ',

        'infix:<eq>' => ' == ',
        'infix:<ne>' => ' != ',
        'infix:<le>' => ' <= ',
        'infix:<ge>' => ' >= ',

        'infix:<==>' => ' == ',
        'infix:<!=>' => ' != ',
        'infix:<=>>' => ', ',
    );

    my %op_global_js = (
        'index'   => 1,
        'die'     => 1,
        'shift'   => 1,
        'unshift' => 1,
        'push'    => 1,
        'pop'     => 1,
        'chr'     => 1,
        'say'     => 1,
        'print'   => 1,
        'warn'    => 1,
    );

    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {

        my $apply = self.op_assign();
        if $apply {
            return $apply.emit_javascript_indented( $level );
        }

        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return Javascript::tab($level) ~ '(' ~ $.code.emit_javascript() ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        }
        if exists %op_infix_js{$code} {
            return Javascript::tab($level) ~ '(' ~ (@.arguments.>>emit_javascript).join( %op_infix_js{$code} ) ~ ')'
        }

        if $code eq 'self'       { return Javascript::tab($level) ~ 'v_self' }
        if $code eq 'Mu'         { return Javascript::tab($level) ~ 'null' }
        if $code eq 'make'       { return Javascript::tab($level) ~ '(v_MATCH.v_capture = ' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')' }
        if $code eq 'defined'    { return Javascript::tab($level) ~ '('  ~ (@.arguments.>>emit_javascript).join(' ')    ~ ' != null)' }
        if $code eq 'substr' {
            return '(' ~ (@.arguments[0]).emit_javascript()
                 ~ ' || "").substr(' ~ (@.arguments[1]).emit_javascript()
                 ~ ( defined(@.arguments[2]) ?? ', ' ~ (@.arguments[2]).emit_javascript() !! '' )
                 ~ ')'
        }

        if $code eq 'chr'        { return 'String.fromCharCode(' ~ Javascript::escape_function('num') ~ '(' ~ (@.arguments[0]).emit_javascript() ~ '))' }
        if $code eq 'ord'        { return '(' ~ (@.arguments[0]).emit_javascript() ~ ').charCodeAt(0)' }

        if $code eq 'Int'        { return 'parseInt(' ~ (@.arguments[0]).emit_javascript() ~ ')' }
        if $code eq 'Num'        { return 'parseFloat(' ~ (@.arguments[0]).emit_javascript() ~ ')' }
        if $code eq 'prefix:<~>' { return Javascript::escape_function('string') ~ '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' }
        if $code eq 'prefix:<!>' { return '( ' ~ Javascript::escape_function('bool') ~ '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? false : true)' }
        if $code eq 'prefix:<?>' { return '( ' ~ Javascript::escape_function('bool') ~ '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? true : false)' }
        if $code eq 'prefix:<$>' { return Javascript::escape_function('scalar') ~ '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' }
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' };  # .' ~ Javascript::escape_function('array') ~ '()' }
        if $code eq 'prefix:<%>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ').' ~ Javascript::escape_function('hash') ~ '()' }

        if $code eq 'postfix:<++>' { return '('   ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')++' }
        if $code eq 'postfix:<-->' { return '('   ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')--' }
        if $code eq 'prefix:<++>'  { return '++(' ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')' }
        if $code eq 'prefix:<-->'  { return '--(' ~ (@.arguments.>>emit_javascript).join(' ')  ~ ')' }

        if $code eq 'infix:<x>'  { return 'str_replicate(' ~ (@.arguments.>>emit_javascript).join(', ')  ~ ')' }

        if $code eq 'list:<~>'   { return '(' ~ Javascript::escape_function('string') ~ '(' ~ (@.arguments.>>emit_javascript()).join(  ') + ' ~ Javascript::escape_function('string') ~ '('  ) ~ '))' }
        if $code eq 'infix:<+>'  { return Javascript::escape_function('add') ~ '('  ~ (@.arguments.>>emit_javascript).join(', ')  ~ ')' }

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
            return Javascript::escape_function('and') ~ '('
                ~ @.arguments[0].emit_javascript() ~ ', '
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })'
        }
        if   $code eq 'infix:<||>'
          || $code eq 'infix:<or>'
        {
            return Javascript::escape_function('or') ~ '('
                ~ @.arguments[0].emit_javascript() ~ ', '
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })'
        }
        if $code eq 'infix:<//>' { return Javascript::escape_function('defined_or') ~ '('
                ~ @.arguments[0].emit_javascript() ~ ', '
                ~ 'function () { return ' ~ @.arguments[1].emit_javascript() ~ '; })'
        }
        if $code eq 'infix:<===>' {
            return '(' ~ Javascript::escape_function('id') ~ '(' ~ (@.arguments[0]).emit_javascript() ~ ') == ' ~ Javascript::escape_function('id') ~ '(' ~ (@.arguments[1]).emit_javascript() ~ '))'
        }

        if $code eq 'exists'     {
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_javascript() ~ ').hasOwnProperty(' ~ ($arg.index_exp).emit_javascript() ~ ')';
            }
        }
        if $code eq 'ternary:<?? !!>' {
            return '( ' ~ Javascript::escape_function('bool') ~ '(' ~ (@.arguments[0]).emit_javascript() ~ ')'
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

        if $.namespace {
            $code = Main::to_javascript_namespace($.namespace) ~ '.' ~ Javascript::escape_function( $code );
        }
        elsif !exists( %op_global_js{$code} ) {
            $code = 'v__NAMESPACE.' ~ Javascript::escape_function( $code );
        }
        else {
            $code = Javascript::escape_function( $.code );
        }
        Javascript::tab($level) ~ $code ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }

    sub emit_javascript_bind ($parameters, $arguments) {
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
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $cond = $.cond;
        if   $cond.isa( 'Var' )
          && $cond.sigil eq '@'
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        }
        my $body      = Perlito5::Javascript::LexicalBlock.new( block => $.body.stmts, needs_return => 0 );
        my $s = Javascript::tab($level) ~ 'if ( ' ~ Javascript::escape_function('bool') ~ '(' ~ $cond.emit_javascript() ~ ') ) { '
            ~ '(function () {' ~ "\n"
            ~       $body.emit_javascript_indented( $level + 1 ) ~ "\n"
            ~ Javascript::tab($level) ~ '})(); }';
        if $.otherwise {
            my $otherwise = Perlito5::Javascript::LexicalBlock.new( block => $.otherwise.stmts, needs_return => 0 );
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
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $body      = Perlito5::Javascript::LexicalBlock.new( block => @.body.stmts, needs_return => 0 );
        return
           Javascript::tab($level) ~ 'for ( '
        ~  ( $.init     ?? $.init.emit_javascript()             ~ '; '  !! '; ' )
        ~  ( $.cond     ?? Javascript::escape_function('bool') ~ '(' ~ $.cond.emit_javascript() ~ '); ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_javascript()         ~ ' '   !! ' '  )
        ~  ') { '
            ~ '(function () { ' ~ $body.emit_javascript_indented( $level + 1 )      ~ ' })()'
        ~ ' }'
    }
}

class For {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $cond = $.cond;
        if !( $cond.isa( 'Var' ) && $cond.sigil eq '@' ) {
            $cond = Lit::Array.new( array1 => [$cond] )
        }
        my $body      = Perlito5::Javascript::LexicalBlock.new( block => @.body.stmts, needs_return => 0 );
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

class Method {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $sig = $.sig;
        my $invocant = $sig.invocant;
        my $pos = $sig.positional;
        my $str = $pos.>>emit_javascript.join(', ');
          Javascript::tab($level) ~ 'function ' ~ $.name ~ '(' ~ $str ~ ') {' ~ "\n"
        ~   (Perlito5::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
        ~ Javascript::tab($level) ~ '}'
    }
}

class Sub {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = $pos.>>emit_javascript.join(', ');
          Javascript::tab($level) ~ 'function ' ~ $.name ~ '(' ~ $str ~ ') {' ~ "\n"
        ~   (Perlito5::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
        ~ Javascript::tab($level) ~ '}'
    }
}

class Do {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        my $block = self.simplify.block;
        return
              Javascript::tab($level) ~ '(function () { ' ~ "\n"
            ~   (Perlito5::Javascript::LexicalBlock.new( block => $block, needs_return => 1 )).emit_javascript_indented( $level + 1 ) ~ "\n"
            ~ Javascript::tab($level) ~ '})()'
    }
}

class Use {
    method emit_javascript { self.emit_javascript_indented(0) }
    method emit_javascript_indented( $level ) {
        Javascript::tab($level) ~ '// use ' ~ $.mod ~ "\n"
    }
}

=begin

=head1 NAME

Perlito5::Javascript::Emit - Code generator for Perlito-in-Javascript

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

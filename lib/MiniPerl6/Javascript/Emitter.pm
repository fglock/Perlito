use v6;

class MiniPerl6::Javascript::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_javascript {
        if !(@.block) {
            return 'null';
        }
        my $str = '';
        for @.block -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $str = $str ~ $decl.emit_javascript_init; 
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $str = $str ~ 'var ' ~ (($decl.parameters).var).emit_javascript ~ ';'; 
            }
        }
        my $last_statement;
        if $.needs_return {
            $last_statement = pop @.block;
        }
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str = $str ~ ($decl).emit_javascript ~ ';';
            }
        }; 
        if $.needs_return && $last_statement {
            if $last_statement.isa( 'If' ) {
                my $cond      = $last_statement.cond;
                my $body      = $last_statement.body;
                my $otherwise = $last_statement.otherwise;
                if $cond.isa( 'Apply' ) && $cond.code eq 'prefix:<!>' {
                    $cond      = ($cond.arguments)[0];
                    $body      = $last_statement.otherwise;
                    $otherwise = $last_statement.body;
                }
                if $cond.isa( 'Var' ) && $cond.sigil eq '@' {
                    $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
                };
                $body      = MiniPerl6::Javascript::LexicalBlock.new( block => $body, needs_return => 1 );
                $otherwise = MiniPerl6::Javascript::LexicalBlock.new( block => $otherwise, needs_return => 1 );
                $str = $str 
                    ~ 'if ( f_bool(' ~ $cond.emit_javascript ~ ') ) { ' 
                        ~ $body.emit_javascript ~ ' } else { ' 
                        ~ $otherwise.emit_javascript ~ ' }';
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                # Return, For - no changes for now 
                $str = $str ~ $last_statement.emit_javascript
            }
            else {
                # $last_statement = Return.new( result => $last_statement );
                $str = $str ~ 'return(' ~ $last_statement.emit_javascript ~ ')'
            }
        }
        if $.top_level {
            $str =  
                  'try { ' ~ $str ~ ' } catch(err) { '
                  ~ 'if ( err instanceof Error ) { '
                    ~ 'throw(err) '
                  ~ '} '
                  ~ 'else { '
                    ~ 'return(err) '
                  ~ '} '
                ~ '} ';
        }
        return $str;
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_javascript {
        my $class_name = Main::to_javascript_namespace($.name);
        my $str =
              '// class ' ~ $.name ~ Main.newline
            ~ 'if (typeof ' ~ $class_name ~ ' != \'object\') {' ~ Main.newline;
        $str = $str  
            ~ '  ' ~ $class_name ~ ' = function() {};' ~ Main.newline
            ~ '  ' ~ $class_name ~ ' = new ' ~ $class_name ~ ';' ~ Main.newline;
        $str = $str  
            ~ '  ' ~ $class_name ~ '.f_isa = function (s) { return s == \'' ~ $.name ~ '\' };' ~ Main.newline
            ~ '  ' ~ $class_name ~ '.f_perl = function () { return \'' ~ $.name ~ '.new(\' + Main._dump(this) + \')\' };' ~ Main.newline;
        $str = $str  
            ~ '}' ~ Main.newline
            ~ '(function () {' ~ Main.newline
            ~ '  var v__NAMESPACE = ' ~ $class_name ~ ';' ~ "\n";

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $str = $str ~ $decl.emit_javascript_init; 
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $str = $str ~ '  var ' ~ (($decl.parameters).var).emit_javascript ~ ';' ~ Main.newline; 
            }
        }

        # $str = $str ~ 'function ' ~ Main::to_javascript_namespace($.name) ~ '() {' ~ Main.newline;

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  
              ~ '  // accessor ' ~ ($decl.var).name ~ Main.newline;
                $str = $str  
              ~ '  ' ~ $class_name ~ '.v_' ~ ($decl.var).name ~ ' = null;' ~ Main.newline;
                $str = $str  
              ~ '  ' ~ $class_name ~ '.f_' ~ ($decl.var).name 
                    ~ ' = function () { return this.v_' ~ ($decl.var).name ~ ' }' ~ Main.newline;
            }
            if $decl.isa( 'Method' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $invocant = $sig.invocant;
                my $block    = MiniPerl6::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // method ' ~ $decl.name ~ Main.newline
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name;
                $str = $str  
                    ~ ' = function (' ~ ((@$pos).>>emit_javascript).join(', ') ~ ') {' ~ Main.newline
              ~ '    var ' ~ $invocant.emit_javascript ~ ' = this;' ~ Main.newline;
                $str = $str  
              ~ '    ' ~ $block.emit_javascript ~ Main.newline
              ~ '  }' ~ Main.newline;
                $str = $str  
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name ~ ';  // v8 bug workaround' ~ Main.newline;
            }
            if $decl.isa( 'Sub' ) {
                my $sig      = $decl.sig;
                my $pos      = $sig.positional;
                my $block    = MiniPerl6::Javascript::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // sub ' ~ $decl.name ~ Main.newline
              ~ '  ' ~ $class_name ~ '.f_' ~ $decl.name;
                $str = $str  
                    ~ ' = function (' ~ ((@$pos).>>emit_javascript).join(', ') ~ ') {' ~ Main.newline
              ~ '    ' ~ $block.emit_javascript ~ Main.newline;
                $str = $str  
              ~ '  }' ~ Main.newline;
            }
        }; 
        # $str = $str ~ '}' ~ Main.newline;
        # $str = $str ~ Main::to_javascript_namespace($.name) 
        #              ~ ' = new ' ~ Main::to_javascript_namespace($.name) ~ ';' ~ Main.newline;
        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str = $str ~ ($decl).emit_javascript ~ ';';
            }
        }; 

        $str = $str ~ '}' 
            ~ ')();' ~ Main.newline;
    }

}

class Val::Int {
    has $.int;
    method emit_javascript { $.int }
}

class Val::Bit {
    has $.bit;
    method emit_javascript { $.bit ?? 'true' !! 'false' }
}

class Val::Num {
    has $.num;
    method emit_javascript { $.num }
}

class Val::Buf {
    has $.buf;
    method emit_javascript { '"' ~ Main::javascript_escape_string($.buf) ~ '"' }
}

class Val::Undef {
    method emit_javascript { 'null' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_javascript {
        die 'Val::Object - not used yet';
    }
}

class Lit::Array {
    has @.array1;
    method emit_javascript {
        my $needs_interpolation = 0;
        for @.array1 -> $item {
            if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' ) 
            {
                $needs_interpolation = 1;
            }
        }
        if $needs_interpolation {
            my $s = '';
            for @.array1 -> $item {
                if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                    || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' ) 
                {
                    $s = $s 
                        ~ '(function(a_) { ' 
                            ~ 'for (var i_ = 0; i_ < a_.length ; i_++) { a.push(a_[i_]) }' 
                        ~ '})(' ~ $item.emit_javascript ~ '); '
                }
                else {
                    $s = $s ~ 'a.push(' ~ $item.emit_javascript ~ '); '
                }
            }
            '(function () { var a = []; ' 
                ~ $s 
            ~ ' return a })()';
        }
        else {
            '[' ~ (@.array1.>>emit_javascript).join(', ') ~ ']';
        }
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_javascript {
        my $needs_interpolation = 0;
        for @.hash1 -> $item {
            if !( ($item[0]).isa( 'Val::Buf' ) ) {
                $needs_interpolation = 1;
            }
        }
        if $needs_interpolation {
            my $s = '';
            for @.hash1 -> $field { 
                $s = $s ~ 'a[' ~ ($field[0]).emit_javascript ~ '] = ' ~ ($field[1]).emit_javascript ~ '; '
            }
            return '(function () { var a = []; ' 
                    ~ $s 
                ~ ' return a })()';
        }
        else {
            my $str = '';
            for @.hash1 -> $field { 
                $str = $str ~ ($field[0]).emit_javascript ~ ':' ~ ($field[1]).emit_javascript ~ ',';
            } 
            return '{ ' ~ $str ~ ' }';
        }
    }
}

class Lit::Code {
    # XXX
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_javascript {
        my $fields = @.fields;
        my $str = '';
        for @$fields -> $field { 
            $str = $str ~ 'v_' ~ ($field[0]).buf ~ ': ' ~ ($field[1]).emit_javascript ~ ',';
        }; 
          'function () { ' 
        ~   'var tmp = {' ~ $str ~ '}; '
        ~   'tmp.__proto__ = ' ~ Main::to_javascript_namespace($.class) ~ '; '
        ~   'return tmp '
        ~ '}()'
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_javascript {
        $.obj.emit_javascript ~ '[' ~ $.index_exp.emit_javascript ~ ']';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_javascript {
        $.obj.emit_javascript ~ '[' ~ $.index_exp.emit_javascript ~ ']';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_javascript {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table = {
            '$' => 'v_',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        };
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
    };
    method plain_name {
        if $.namespace {
            return $.namespace ~ '.' ~ $.name
        }
        return $.name
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_javascript {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $.parameters.array1;
            #my $b = $.arguments.array1;
            my $str = 'do { ';
            my $i = 0;
            for @$a -> $var { 
                my $bind = Bind.new( 
                    parameters => $var, 
                    # arguments => ($b[$i]) );
                    arguments  => Index.new(
                        obj    => $.arguments,
                        index_exp  => Val::Int.new( int => $i )
                    )
                );
                $str = $str ~ ' ' ~ $bind.emit_javascript ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_javascript ~ ' }';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} = { a => 1, b => [2, 3]}

            my $a = $.parameters.hash1;
            my $b = $.arguments.hash1;
            my $str = 'do { ';
            my $i = 0;
            my $arg;
            for @$a -> $var {

                $arg = Val::Undef.new();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg = $var2[1];
                    }
                };

                my $bind = Bind.new( parameters => $var[1], arguments => $arg );
                $str = $str ~ ' ' ~ $bind.emit_javascript ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_javascript ~ ' }';
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  Obj.new(:$a, :$b) = $obj

            my $class = $.parameters.class;
            my $a     = $.parameters.fields;
            my $b     = $.arguments;
            my $str   = 'do { ';
            my $i     = 0;
            my $arg;
            for @$a -> $var {
                my $bind = Bind.new( 
                    parameters => $var[1], 
                    arguments  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str = $str ~ ' ' ~ $bind.emit_javascript ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_javascript ~ ' }';
        };
    
        if $.parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return '(' ~ ($.parameters.invocant).emit_javascript ~ '.v_' ~ $.parameters.method ~ ' = ' ~ $.arguments.emit_javascript ~ ')';
        }

        '(' ~ $.parameters.emit_javascript ~ ' = ' ~ $.arguments.emit_javascript ~ ')';
    }
}

class Proto {
    has $.name;
    method emit_javascript {
        Main::to_javascript_namespace($.name)        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_javascript {
        my $invocant = $.invocant.emit_javascript;
        if $invocant eq 'self' {
            $invocant = 'v_self';
        }
        if     ($.method eq 'perl')
            || ($.method eq 'isa')
            || ($.method eq 'scalar')
            || ($.method eq 'keys')
            || ($.method eq 'values')
        { 
            if ($.hyper) {
                return 
                    '(function (a_) { '
                        ~ 'var out = []; ' 
                        ~ 'if ( typeof a_ == \'undefined\' ) { return out }; ' 
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
           || ($.method eq 'push') 
           || ($.method eq 'pop') 
        {
            return $invocant ~ '.' ~ $.method ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }

        if     ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'chars')
        { 
            if ($.hyper) {
                return 
                    '(function (a_) { '
                        ~ 'var out = []; ' 
                        ~ 'if ( typeof a_ == \'undefined\' ) { return out }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( Main.' ~ $.method ~ '(a_[i]) ) } return out;'
                    ~ ' })(' ~ $invocant ~ ')'
            }
            else {
                if @.arguments {
                    return
                        'Main.' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
                }
                else {
                    return
                        'Main.' ~ $.method ~ '(' ~ $invocant ~ ')';
                }
            }
        }

        my $meth = $.method;
        
        if ($.hyper) {
            return
                    '(function (a_) { '
                        ~ 'var out = []; ' 
                        ~ 'if ( typeof a_ == \'undefined\' ) { return out }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.push( a_[i].f_' ~ $meth ~ '() ) } return out;'
                    ~ ' })(' ~ $invocant ~ ')'
        }
        if  $meth eq 'postcircumfix:<( )>'  {
            return '(' ~ $invocant ~ ')(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
        }
        return $invocant ~ '.f_' ~ $meth ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_javascript {
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_javascript ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        }

        if $code eq 'self'       { return 'v_self' };
        if $code eq 'False'      { return 'false' };
        if $code eq 'True'       { return 'true' };
        if $code eq 'make'       { return '(v_MATCH.v_capture = ' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')' };
        if $code eq 'say'        { return 'f_say('    ~ (@.arguments.>>emit_javascript).join(', ') ~ ')' };
        if $code eq 'print'      { return 'f_print('  ~ (@.arguments.>>emit_javascript).join(', ') ~ ')' };
        if $code eq 'warn'       { return 'f_warn('   ~ (@.arguments.>>emit_javascript).join(' + ') ~ ')' };
        # if $code eq 'array'      { return '@{' ~ (@.arguments.>>emit_javascript).join(' ')    ~ '}' };
        if $code eq 'defined'    { return '('  ~ (@.arguments.>>emit_javascript).join(' ')    ~ ' != null)' };
        if $code eq 'substr' { 
            return '(' ~ (@.arguments[0]).emit_javascript ~
                 ' || "").substr(' ~ (@.arguments[1]).emit_javascript ~
                 ', ' ~ (@.arguments[2]).emit_javascript ~ ')' 
        };
        if $code eq 'Int'        { return 'parseInt(' ~ (@.arguments[0]).emit_javascript ~ ')' };
        if $code eq 'Num'        { return 'parseFloat(' ~ (@.arguments[0]).emit_javascript ~ ')' };

        if $code eq 'prefix:<~>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ').f_string()' };
        if $code eq 'prefix:<!>' { return '( f_bool('  ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? false : true)' };
        if $code eq 'prefix:<?>' { return '( f_bool('  ~ (@.arguments.>>emit_javascript).join(' ')    ~ ') ? true : false)' };
        if $code eq 'prefix:<$>' { return 'f_scalar(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ')' };  # .f_array()' };
        if $code eq 'prefix:<%>' { return '(' ~ (@.arguments.>>emit_javascript).join(' ')    ~ ').f_hash()' };

        if $code eq 'infix:<~>'  { return '( f_string(' ~ (@.arguments[0]).emit_javascript ~ ')'
                                       ~ ' + f_string(' ~ (@.arguments[1]).emit_javascript ~ ') )' };
        if $code eq 'infix:<+>'  { return 'f_add('  ~ (@.arguments.>>emit_javascript).join(', ')  ~ ')' };
        if $code eq 'infix:<->'  { return '('  ~ (@.arguments.>>emit_javascript).join(' - ')   ~ ')' };
        if $code eq 'infix:<*>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' * ')   ~ ')' };
        if $code eq 'infix:</>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' / ')   ~ ')' };
        if $code eq 'infix:<>>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' > ')   ~ ')' };
        if $code eq 'infix:<<>'  { return '('  ~ (@.arguments.>>emit_javascript).join(' < ')   ~ ')' };
        if $code eq 'infix:<>=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' >= ')  ~ ')' };
        if $code eq 'infix:<<=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' <= ')  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '( f_bool(' ~ (@.arguments[0]).emit_javascript ~ ')'
                                      ~ ' && f_bool(' ~ (@.arguments[1]).emit_javascript ~ ') )' };
        if $code eq 'infix:<||>' { return '( f_bool(' ~ (@.arguments[0]).emit_javascript ~ ')'
                                      ~ ' || f_bool(' ~ (@.arguments[1]).emit_javascript ~ ') )' };

        if $code eq 'infix:<eq>' { return '('  ~ (@.arguments.>>emit_javascript).join(' == ')  ~ ')' };
        if $code eq 'infix:<ne>' { return '('  ~ (@.arguments.>>emit_javascript).join(' != ')  ~ ')' };
 
        if $code eq 'infix:<==>' { return '('  ~ (@.arguments.>>emit_javascript).join(' == ')  ~ ')' };
        if $code eq 'infix:<!=>' { return '('  ~ (@.arguments.>>emit_javascript).join(' != ')  ~ ')' };

        if $code eq 'exists'     { 
            my $arg = @.arguments[0];
            if $arg.isa( 'Lookup' ) {
                return '(' ~ ($arg.obj).emit_javascript ~ ').hasOwnProperty(' ~ ($arg.index_exp).emit_javascript ~ ')';
            }
        };

        if $code eq 'ternary:<?? !!>' { 
            return '( f_bool(' ~ (@.arguments[0]).emit_javascript ~ ')' 
                 ~ ' ? ' ~ (@.arguments[1]).emit_javascript 
                 ~ ' : ' ~ (@.arguments[2]).emit_javascript 
                 ~ ')' };
        
        $code = 'f_' ~ $.code;
        if $.namespace {
            $code = Main::to_javascript_namespace($.namespace) ~ '.' ~ $code;
        }
        elsif  ($code ne 'f_index') 
            && ($code ne 'f_die') 
            && ($code ne 'f_pop') 
            && ($code ne 'f_shift') 
            && ($code ne 'f_push') 
        {
            $code = 'v__NAMESPACE.' ~ $code;
        }
        $code ~ '(' ~ (@.arguments.>>emit_javascript).join(', ') ~ ')';
    }
}

class Return {
    has $.result;
    method emit_javascript {
        return
        'throw(' ~ $.result.emit_javascript ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_javascript {
        my $cond = $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if = If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.emit_javascript;
        }
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        my $body      = MiniPerl6::Javascript::LexicalBlock.new( block => @.body, needs_return => 0 );
        my $otherwise = MiniPerl6::Javascript::LexicalBlock.new( block => @.otherwise, needs_return => 0 );
        return
            'if ( f_bool(' ~ $cond.emit_javascript ~ ') ) { ' 
              ~ '(function () { ' ~ $body.emit_javascript      ~ ' })() } else { ' 
              ~ '(function () { ' ~ $otherwise.emit_javascript ~ ' })() }';
    }
}


class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit_javascript {
        my $body      = MiniPerl6::Javascript::LexicalBlock.new( block => @.body, needs_return => 0 );
        return
           'for ( '
        ~  ( $.init     ?? $.init.emit_javascript             ~ '; '  !! '; ' )
        ~  ( $.cond     ?? 'f_bool(' ~ $.cond.emit_javascript ~ '); ' !! '; ' )
        ~  ( $.continue ?? $.continue.emit_javascript         ~ ' '   !! ' '  )
        ~  ') { '
            ~ '(function () { ' ~ $body.emit_javascript      ~ ' })()' 
        ~ ' }'
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_javascript {
        my $body      = MiniPerl6::Javascript::LexicalBlock.new( block => @.body, needs_return => 0 );
        '(function (a_) { for (var i_ = 0; i_ < a_.length ; i_++) { ' 
            ~ '(function (' ~ $.topic.emit_javascript ~ ') { '
                ~ $body.emit_javascript 
            ~ ' })(a_[i_]) } })' 
        ~ '(' ~ $.cond.emit_javascript ~ ')'
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_javascript {
        $.var.emit_javascript;
    }
    method emit_javascript_init {
        if $.decl eq 'my' {
            my $str = "";
            $str = $str ~ 'var ' ~ ($.var).emit_javascript ~ ' = ';
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
    method emit_javascript {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_javascript {
        my $sig = $.sig;
        my $invocant = $sig.invocant; 
        my $pos = $sig.positional;
        my $str = ((@$pos).>>emit_javascript).join(', ');  
        'function ' ~ $.name ~ '(' ~ $str ~ ') { ' ~ 
          (MiniPerl6::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript ~ 
        ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_javascript {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $str = ((@$pos).>>emit_javascript).join(', ');  
        'function ' ~ $.name ~ '(' ~ $str ~ ') { ' ~ 
          (MiniPerl6::Javascript::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_javascript ~ 
        ' }'
    }
}

class Do {
    has @.block;
    method emit_javascript {
        '(function () { ' ~ 
          (MiniPerl6::Javascript::LexicalBlock.new( block => @.block, needs_return => 1 )).emit_javascript ~ 
        ' })()'
    }
}

class Use {
    has $.mod;
    method emit_javascript {
        '// use ' ~ $.mod ~ Main.newline
    }
}

=begin

=head1 NAME 

MiniPerl6::Javascript::Emit - Code generator for MiniPerl6-in-Javascript

=head1 SYNOPSIS

    $program.emit_javascript  # generated Perl5 code

=head1 DESCRIPTION

This module generates Javascript code for the MiniPerl6 compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2006, 2009 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

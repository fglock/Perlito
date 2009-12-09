use v6-alpha;

class MiniPerl6::Go::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_go {
        if !(@.block) {
            return 'null';
        }
        my $str := '';
        for @.block -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                # uninitialized variable
                $str := $str ~ ($decl.var).emit_go ~ ' := new(Scalar);' ~ Main.newline;
                if ($decl.var).sigil eq '%' {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( h_hash() );' ~ Main.newline;
                } 
                else {
                if ($decl.var).sigil eq '@' {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( a_array() );' ~ Main.newline;
                } 
                else {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( u_undef );' ~ Main.newline;
                } 
                }
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $str := $str ~ (($decl.parameters).var).emit_go ~ ' := new(Scalar);'; 
            }
        }
        my $last_statement;
        if $.needs_return {
            $last_statement := pop @.block;
        }
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str := $str ~ ($decl).emit_go ~ ';';
            }
        }; 
        if $.needs_return && $last_statement {
            if $last_statement.isa( 'If' ) {
                my $cond      := $last_statement.cond;
                my $body      := $last_statement.body;
                my $otherwise := $last_statement.otherwise;
                if $cond.isa( 'Apply' ) && $cond.code eq 'prefix:<!>' {
                    $cond      := ($cond.arguments)[0];
                    $body      := $last_statement.otherwise;
                    $otherwise := $last_statement.body;
                }
                if $cond.isa( 'Var' ) && $cond.sigil eq '@' {
                    $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
                };
                $body      := ::MiniPerl6::Go::LexicalBlock( block => $body, needs_return => 1 );
                $otherwise := ::MiniPerl6::Go::LexicalBlock( block => $otherwise, needs_return => 1 );
                $str := $str 
                    ~ 'if ( (' ~ $cond.emit_go ~ ').Bool().b ) { ' 
                        ~ $body.emit_go ~ ' } else { ' 
                        ~ $otherwise.emit_go ~ ' }';
            }
            else {
            if $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                # Return, For - no changes for now 
                $str := $str ~ $last_statement.emit_go
            }
            else {
                # $last_statement := ::Return( result => $last_statement );
                if $.top_level {
                    $str := $str ~ 'Return( p, ' ~ $last_statement.emit_go ~ ')'
                }
                else {
                    $str := $str ~ 'return(' ~ $last_statement.emit_go ~ ')'
                }
            }
            }
        }
        if $.top_level {
            $str :=  
'
    p := make(chan Any);
    go func () { ' ~ $str ~ '; return }();
    return <-p;
';
        }
        return $str;
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_go {
        my $class_name := Main::to_go_namespace($.name);
        my $str :=
              '// instances of class ' ~ $.name ~ Main.newline
            ~ 'type ' ~ $class_name ~ ' struct {' ~ Main.newline;
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  ~ '  ' ~ 'v_' ~ ($decl.var).name ~ ' Scalar;' ~ Main.newline
            }
        }
        $str := $str ~ '}' ~ Main.newline;

        $str := $str 
            ~ '// methods in class ' ~ $.name ~ Main.newline
            ~ 'var Method_' ~ $class_name ~ ' struct {' ~ Main.newline;
        for @.body -> $decl { 
            if $decl.isa( 'Method' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' func (*' ~ $class_name ~ ', Capture) Any;' ~ Main.newline;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ ($decl.var).name ~ ' func (*' ~ $class_name ~ ', Capture) Any;' ~ Main.newline;
            }
        }
        $str := $str ~ '}' ~ Main.newline;

        $str := $str 
            ~ '// namespace ' ~ $.name ~ Main.newline
            ~ 'var Namespace_' ~ $class_name ~ ' struct {' ~ Main.newline;
        for @.body -> $decl { 
            if $decl.isa( 'Sub' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' Function;' ~ Main.newline;
            }
        }
        $str := $str ~ '}' ~ Main.newline;
        $str := $str ~ 'var Run_' ~ $class_name ~ ' func ();' ~ Main.newline;

        $str := $str 
            ~ '// method wrappers for ' ~ $.name ~ Main.newline
        for @.body -> $decl {
            if $decl.isa( 'Method' ) {
                $str := $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ $decl.name ~ ' (v Capture) Any {' ~ Main.newline
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ $decl.name ~ '(v_self, v);'~ Main.newline
                    ~ '}'~ Main.newline;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ ($decl.var).name ~ ' (v Capture) Any {' ~ Main.newline
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name ~ '(v_self, v);'~ Main.newline
                    ~ '}'~ Main.newline;
            }
        }

        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Bool () Bool { return b_true }' ~ Main.newline;
        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Int () Int { panic("converting class to int") }' ~ Main.newline;
        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Str () Str { panic("converting class to string") }' ~ Main.newline;
        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Array () Array { panic("converting class to array") }' ~ Main.newline;
        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Hash () Hash { panic("converting class to hash") }' ~ Main.newline;
        $str := $str ~ 'func (v_self ' ~ $class_name ~ ') Equal (j Any) Bool { panic("comparing class") }' ~ Main.newline;
        $str := $str ~ 'func (v_self *' ~ $class_name ~ ') Fetch () Any { return *v_self }' ~ Main.newline;

        $str := $str 
            ~ '// prototype of ' ~ $.name ~ Main.newline
            ~ 'var Proto_' ~ $class_name ~ ' Scalar;' ~ Main.newline;

        $str := $str ~ 'func Init_' ~ $class_name ~ '() {' ~ Main.newline;

        #if $class_name eq 'Main' {
        #    $str := $str ~ 'func main() {' ~ Main.newline;
        #}
        $str := $str 
            ~ '  this_namespace := &Namespace_' ~ $class_name ~ ';' ~ Main.newline
            ~ '  this_namespace = this_namespace;' ~ Main.newline
            ~ '  Proto_' ~ $class_name ~ '.Bind( new(' ~ $class_name ~ ') );' ~ Main.newline;
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $str := $str ~ '  ' ~ ($decl.var).emit_go ~ ' := new(Scalar);' ~ Main.newline; 
                if ($decl.var).sigil eq '%' {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( h_hash() );' ~ Main.newline;
                } 
                else {
                if ($decl.var).sigil eq '@' {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( a_array() );' ~ Main.newline;
                } 
                else {
                    $str := $str ~ ($decl.var).emit_go ~ '.Bind( u_undef );' ~ Main.newline;
                } 
                }
                $str := $str ~ Main.newline;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $str := $str ~ '  ' ~ (($decl.parameters).var).emit_go ~ ' := new(Scalar);' ~ Main.newline; 
            }
        }

        # $str := $str ~ 'func ' ~ Main::to_go_namespace($.name) ~ '() {' ~ Main.newline;

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  
              ~ '  // accessor ' ~ ($decl.var).name ~ Main.newline
              ~ '  Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name 
                    ~ ' = func (v_self *' ~ $class_name ~ ', v Capture) Any {' ~ Main.newline
              ~ '    ' ~ 'return v_self.v_' ~ ($decl.var).name ~ Main.newline
              ~ '  };' ~ Main.newline;
            }
            if $decl.isa( 'Method' ) {
                my $sig      := $decl.sig;
                my $block    := ::MiniPerl6::Go::LexicalBlock( block => $decl.block, needs_return => 1, top_level => 1 );
                $str := $str 
              ~ '  // method ' ~ $decl.name ~ Main.newline
              ~ '  Method_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = func (' ~ ($sig.invocant).emit_go ~ ' *' ~ $class_name ~ ', v Capture) Any {' ~ Main.newline
              ~ '    ' ~ $sig.emit_go_bind ~ Main.newline
              ~ '    ' ~ $block.emit_go ~ Main.newline
              ~ '  };' ~ Main.newline
            }
            if $decl.isa( 'Sub' ) {
                my $sig      := $decl.sig;
                my $block    := ::MiniPerl6::Go::LexicalBlock( block => $decl.block, needs_return => 1, top_level => 1 );
                $str := $str 
              ~ '  // sub ' ~ $decl.name ~ Main.newline
              ~ '  Namespace_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = Function{ f : func (v Capture) Any {' ~ Main.newline
              ~ '    ' ~ $sig.emit_go_bind ~ Main.newline
              ~ '    ' ~ $block.emit_go ~ Main.newline
              ~ '  } };' ~ Main.newline;
            }
        }; 

        $str := $str 
            ~ '  // main runtime block of ' ~ $.name ~ Main.newline
            ~ '  Run_' ~ $class_name ~ ' = func () {' ~ Main.newline;
        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str := $str ~ '    ' ~ ($decl).emit_go ~ ';' ~ Main.newline;
            }
        }; 
        $str := $str ~ '  }' ~ Main.newline;

        $str := $str ~ '}' ~ Main.newline;
        return $str;
    }

}

class Val::Int {
    has $.int;
    method emit_go { 'Int{i:' ~ $.int ~ '}' }
}

class Val::Bit {
    has $.bit;
    method emit_go { 'Bit{b:' ~ $.bit ~ '}' }
}

class Val::Num {
    has $.num;
    method emit_go { 'Num{n:' ~ $.num ~ '}' }
}

class Val::Buf {
    has $.buf;
    method emit_go { 'Str{s:"' ~ Main::javascript_escape_string($.buf) ~ '"}' }
}

class Val::Undef {
    method emit_go { 'u_undef' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_go {
        die 'Val::Object - not used yet';
    }
}

class Lit::Seq {
    has @.seq;
    method emit_go {
        '[]Any{ '
            ~ (@.seq.>>emit_go).join(', ') 
        ~ ' }';
    }
}

class Lit::Array {
    has @.array;
    method emit_go {
        my $str := '';
        for @.array -> $item {
            if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' ) 
            {
                $str := $str 
                    ~ 'func(a_) { ' 
                        ~ 'for i_ := 0; i_ < a_.length ; i_++ { a.Push(a_[i_]) }' 
                    ~ '}(' ~ $item.emit_go ~ '); '
            }
            else {
                $str := $str ~ 'a.Push(' ~ $item.emit_go ~ '); '
            }
        }
        'func () Array { ' 
                ~ 'a := a_array(); '
                ~ $str 
                ~ 'return a; '
        ~ '}()'
    }
}

class Lit::Hash {
    has @.hash;
    method emit_go {
        my $fields := @.hash;
        my $str := ''; 
        for @$fields -> $field { 
            $str := $str 
                ~ 'm[' ~ ($field[0]).emit_go ~ '] = ' ~ ($field[1]).emit_go ~ '; ';
        }; 
        'func() map[string]Any { ' 
            ~ 'var m = make(map[string]Any); '
            ~ $str 
            ~ 'return m; '
        ~ '}()';
    }
}

class Lit::Code {
    # XXX
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_go {
        my $fields := @.fields;
        my $str := '';
        for @$fields -> $field { 
            $str := $str 
                ~ 'm.v_' ~ ($field[0]).buf ~ '.Bind( ' ~ ($field[1]).emit_go ~ ' ); ';
        }; 
        'func() *' ~ Main::to_go_namespace($.class) ~ ' { ' 
            ~ 'var m = new(' ~ Main::to_go_namespace($.class) ~ '); '
            ~ $str 
            ~ 'return m; '
        ~ '}()';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_go {
        $.obj.emit_go ~ '.Array().Index(' ~ $.index_exp.emit_go ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_go {
        $.obj.emit_go ~ '.Hash().Lookup(' ~ $.index_exp.emit_go ~ ')';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_go {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $table := {
            '$' => 'v_',
            '@' => 'List_',
            '%' => 'Hash_',
            '&' => 'Code_',
        };
        my $ns := '';
        if $.namespace {
            $ns := Main::to_go_namespace($.namespace) ~ '.';
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
    method emit_go {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] := [1, [2, 3]]
            
            my $a := $.parameters.array;
            #my $b := $.arguments.array;
            my $str := 
                'func () Any { '
                    ~ 'List_tmp := ' ~ $.arguments.emit_go ~ '; ';
            my $i := 0;
            for @$a -> $var { 
                my $bind := ::Bind( 
                    'parameters' => $var, 
                    'arguments'  => ::Index(
                        obj        => ::Var( sigil => '@', twigil => '', namespace => '', name => 'tmp' ),
                        index_exp  => ::Val::Int( int => $i )
                    )
                );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ ' return List_tmp }()';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} := { a => 1, b => [2, 3]}

            my $a := $.parameters.hash;
            my $b := $.arguments.hash;
            my $str := 'do { ';
            my $i := 0;
            my $arg;
            for @$a -> $var {

                $arg := ::Val::Undef();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg := $var2[1];
                    }
                };

                my $bind := ::Bind( 'parameters' => $var[1], 'arguments' => $arg );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit_go ~ ' }';
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  ::Obj(:$a, :$b) := $obj

            my $class := $.parameters.class;
            my $a     := $.parameters.fields;
            my $b     := $.arguments;
            my $str   := 'do { ';
            my $i     := 0;
            my $arg;
            for @$a -> $var {
                my $bind := ::Bind( 
                    'parameters' => $var[1], 
                    'arguments'  => ::Call( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit_go ~ ' }';
        };
    
        if $.parameters.isa( 'Call' ) {
            # $var.attr := 3;
            return 
                'func () Any { ' 
                    ~ 'var tmp Scalar = ' ~ ($.parameters.invocant).emit_go 
                        ~ '.Fetch().(' ~ $.parameters.method ~ '_er)'
                        ~ '.f_' ~ $.parameters.method ~ '( Capture{ p : []Any{  } } ).(Scalar); '
                    ~ 'tmp.Bind( ' ~ $.arguments.emit_go ~ ' ); '
                    ~ 'return tmp; '
                ~ '}()';
        }

        $.parameters.emit_go ~ '.Bind( ' ~ $.arguments.emit_go ~ ' )';
    }
}

class Proto {
    has $.name;
    method emit_go {
        Main::to_go_namespace($.name)        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_go {
        my $invocant := $.invocant.emit_go;
        if ($.invocant).isa( 'Proto' ) {
            $invocant := 'Proto_' ~ $invocant
        }

        if     ($.method eq 'values')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return '@{' ~ $invocant ~ '}';
            }
        };

        if     ($.method eq 'perl')
            || ($.method eq 'isa')
            || ($.method eq 'scalar')
        { 
            if ($.hyper) {
                return 
                    'func (a_ Any) Array { '
                        ~ 'var out = a_array(); ' 
                        ~ 'if ( typeof a_ == \'undefined\' ) { return out }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.Push( f_' ~ $.method ~ '(a_[i]) ) } return out;'
                    ~ ' }(' ~ $invocant ~ ')'
            }
            return 'f_' ~ $.method ~ '(' 
                    ~ $invocant 
                    ~ ( @.arguments ?? ', ' ~ (@.arguments.>>emit_go).join(', ') !! '' ) 
                ~ ')';
        }
        # if ($.method eq 'join') {
        #    return $invocant ~ '.Join( Capture{ p : []Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
        # }

        if     ($.method eq 'yaml')
            || ($.method eq 'say' )
            || ($.method eq 'chars')
        { 
            if ($.hyper) {
                return 
                    'func (a_ Any) Array { '
                        ~ 'var out = a_array(); ' 
                        ~ 'if ( typeof a_ == \'undefined\' ) { return out }; ' 
                        ~ 'for(var i = 0; i < a_.length; i++) { '
                            ~ 'out.Push( Main.' ~ $.method ~ '(a_[i]) ) } return out;'
                    ~ ' }(' ~ $invocant ~ ')'
            }
            else {
                if defined @.arguments {
                    return
                        'Main.' ~ $.method ~ '(' ~ $invocant ~ ', ' ~ (@.arguments.>>emit_go).join(', ') ~ ')';
                }
                else {
                    return
                        'Main.' ~ $.method ~ '(' ~ $invocant ~ ')';
                }
            }
        };

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
            if ($.hyper) {
                $meth := '';  # ???
            }
            else {
                return $invocant ~ '.Apply( Capture{ p : []Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
            }
        };
        
        if ($.hyper) {
                    'func (a_ Any) Array { '
                        ~ 'var out = a_array(); ' 
                        ~ 'var i Array = a_.Array(); '
                        ~ 'for pos := 0; pos < i.n; pos++ { '
                            ~ 'out.Push( i.v[pos].Fetch().(' ~ $meth ~ '_er).f_' ~ $meth ~ '(Capture{ p : []Any{}  })) } '
                        ~ 'return out; '
                    ~ '}(' ~ $invocant ~ ')'
        }
        else {
            $invocant ~ '.Fetch().(' ~ $meth ~ '_er).f_' ~ $meth ~ '( Capture{ p : []Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_go {
        my $code := $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_go ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'v_self' };
        if $code eq 'false'      { return 'b_false' };
        if $code eq 'make'       { 
            return 
                  'func () Any { ' 
                    ~ 'tmp := ' ~ (@.arguments.>>emit_go).join(', ') ~ '; '
                    ~ 'v_MATCH.v_capture = tmp; '
                    ~ 'return tmp; '
                ~ '}()';
        };

        if $code eq 'say'           { return 'Print( Capture{ p : []Any{ '    
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ', Str{s:"\n"} } } )'
                                    }
        if $code eq 'print'         { return 'Print( Capture{ p : []Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'warn'          { return 'Print_stderr( Capture{ p : []Any{ '   
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ', Str{s:"\n"} } } )' 
                                    }
        # if $code eq 'array'    { return '@{' ~ (@.arguments.>>emit_go).join(' ')    ~ '}' };
        if $code eq 'defined'    { return '('  ~ (@.arguments.>>emit_go).join(' ')    ~ ' != null)' };
        if $code eq 'substr'     { return 'Substr( Capture{ p : []Any{ ' 
                                        ~ (@.arguments.>>emit_go).join(', ') 
                                        ~ ' } } )'  };
        if $code eq 'prefix:<~>' { return '(' ~ (@.arguments.>>emit_go).join(' ')    ~ ').Str()' };
        if $code eq 'prefix:<!>' { return '('  ~ (@.arguments.>>emit_go).join(' ')    ~ ').Bool().Not()' };
        if $code eq 'prefix:<?>' { return '('  ~ (@.arguments.>>emit_go).join(' ')    ~ ').Bool()' };
        if $code eq 'prefix:<$>' { return 'f_scalar(' ~ (@.arguments.>>emit_go).join(' ')    ~ ')' };
        if $code eq 'prefix:<@>' { return '(' ~ (@.arguments.>>emit_go).join(' ')    ~ ')' };  # .f_array()' };
        if $code eq 'prefix:<%>' { return '(' ~ (@.arguments.>>emit_go).join(' ')    ~ ').f_hash()' };

        if $code eq 'infix:<~>'  { return 'Str{ s: strings.Join( []string{' 
                                        ~ '(' ~ (@.arguments[0]).emit_go ~ ').Str().s, ' 
                                        ~ '(' ~ (@.arguments[1]).emit_go ~ ').Str().s' 
                                    ~ '}, "" ) }' 
                                };
        if $code eq 'infix:<+>'  { return 'Int{ i:' 
                                        ~ '(' ~ (@.arguments[0]).emit_go ~ ').Int().i + '
                                        ~ '(' ~ (@.arguments[1]).emit_go ~ ').Int().i ' 
                                    ~ '}' 
                                };
        if $code eq 'infix:<->'  { return 'Int{ i:' 
                                        ~ '(' ~ (@.arguments[0]).emit_go ~ ').Int().i - '
                                        ~ '(' ~ (@.arguments[1]).emit_go ~ ').Int().i '
                                    ~ '}' 
                                };
        if $code eq 'infix:<>>'  { return 'Bool{ b:' 
                                        ~ '(' ~ (@.arguments[0]).emit_go ~ ').Int().i > '
                                        ~ '(' ~ (@.arguments[1]).emit_go ~ ').Int().i '
                                    ~ '}' 
                                };

        if $code eq 'infix:<&&>' { return '( (' ~ (@.arguments[0]).emit_go ~ ').Bool()'
                                      ~ ' && (' ~ (@.arguments[1]).emit_go ~ ').Bool() )' };
        if $code eq 'infix:<||>' { return '( (' ~ (@.arguments[0]).emit_go ~ ').Bool()'
                                      ~ ' || (' ~ (@.arguments[1]).emit_go ~ ').Bool() )' };

        if $code eq 'infix:<eq>' { return '(' ~ (@.arguments[0]).emit_go ~ ').Str().Str_equal('
                                       ~ (@.arguments[1]).emit_go ~ ')' };
        if $code eq 'infix:<ne>' { return '(' ~ (@.arguments[0]).emit_go ~ ').Str().Str_equal('
                                       ~ (@.arguments[1]).emit_go ~ ').Not()' };
 
        if $code eq 'infix:<==>' { return '(' ~ (@.arguments[0]).emit_go ~ ').Equal('
                                       ~ (@.arguments[1]).emit_go ~ ')' };
        if $code eq 'infix:<!=>' { return '(' ~ (@.arguments[0]).emit_go ~ ').Equal('
                                       ~ (@.arguments[1]).emit_go ~ ').Not()' };

        if $code eq 'ternary:<?? !!>' { 
            return 
                'func () Any { '
                    ~ 'if (' ~ (@.arguments[0]).emit_go ~ ').Bool().b ' 
                    ~ '{ return ' ~ (@.arguments[1]).emit_go ~ ' }; '
                    ~ 'return ' ~ (@.arguments[2]).emit_go ~ ' '
                ~ '}()'
        };
        
        $code := 'f_' ~ $.code;
        if $.namespace {
            $code := 'Namespace_' ~ Main::to_go_namespace($.namespace) ~ '.' ~ $code;
        }
        else {
            $code := 'this_namespace.' ~ $code;
        }
        $code ~ '.Apply( Capture{ p : []Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
    }
}

class Return {
    has $.result;
    method emit_go {
        return
        'Return( p, ' ~ $.result.emit_go ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_go {
        my $cond := $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if := ::If( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.emit_go;
        }
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := ::Apply( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        'if ( (' ~ $cond.emit_go ~ ').Bool().b ) { ' 
            ~ (@.body.>>emit_go).join(';') ~ ' } else { ' 
            ~ (@.otherwise.>>emit_go).join(';') ~ ' }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_go {
        'func (a_ Any) { for i_ := 0; i_ < len(a_); i_++ { ' 
            ~ 'func (' ~ $.topic.emit_go ~ ' Any) { '
                ~ (@.body.>>emit_go).join(';') 
            ~ ' }(a_[i_]) } }' 
        ~ '(' ~ $.cond.emit_go ~ ')'
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_go {
        $.var.emit_go;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_go {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    }
    method emit_go_bind {
        my $str := '';
        my $i := 0;
        for @($.positional) -> $decl { 
            $str := $str ~ $decl.emit_go ~ ' := v.p[' ~ $i ~ ']; ';
            $i := $i + 1;
        }
        return $str;
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_go {
        my $invocant := ($.sig).invocant; 
        'func ' ~ $.name ~ '(v Capture) Any { ' 
              ~ '    ' ~ ($.sig).emit_go_bind ~ Main.newline
              ~ '    ' ~ ::MiniPerl6::Go::LexicalBlock( block => @.block, needs_return => 1, top_level => 1 ).emit_go 
        ~ ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_go {
        if $.name eq '' { 
            return
                'Function{ f: func(v Capture) Any { '
                    ~ '    ' ~ ($.sig).emit_go_bind ~ Main.newline
                    ~ '    ' ~ ::MiniPerl6::Go::LexicalBlock( block => @.block, needs_return => 1, top_level => 1 ).emit_go 
                    ~ '} '
                ~ '}'
        }

        'func ' ~ $.name ~ '(v Capture) Any { ' 
              ~ '    ' ~ ($.sig).emit_go_bind ~ Main.newline
              ~ '    ' ~ ::MiniPerl6::Go::LexicalBlock( block => @.block, needs_return => 1, top_level => 1 ).emit_go 
        ~ ' }'
    }
}

class Do {
    has @.block;
    method emit_go {
        '(func () Any { ' 
          ~ ::MiniPerl6::Go::LexicalBlock( block => @.block, needs_return => 1 ).emit_go 
          ~ '; return u_undef '
        ~ '})()'
    }
}

class Use {
    has $.mod;
    method emit_go {
        '// use ' ~ $.mod ~ Main.newline
    }
}

=begin

=head1 NAME 

MiniPerl6::Go::Emit - Code generator for MiniPerl6-in-Go

=head1 SYNOPSIS

    $program.emit_go  # generated Go code

=head1 DESCRIPTION

This module generates Go code for the MiniPerl6 compiler.

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

use v6;

class Perlito::Java::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_java {
        if !(@.block) {
            return '';
        }
        my $str = '';
        my %decl_seen;
        for @.block -> $decl1 { 
            my $decl = $decl1;
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $decl = $decl.parameters;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                my $var_name = (($decl).var).emit_java;
                if !(%decl_seen{ $var_name }) {
                    $str = $str ~ $decl.emit_java_init;
                    %decl_seen{ $var_name } = 1;
                }
            }
        }
        my $last_statement;
        if $.needs_return {
            $last_statement = pop @.block;
        }
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str = $str ~ ($decl).emit_java ~ ';';
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
                $body      = Perlito::Java::LexicalBlock.new( block => $body, needs_return => 1, top_level => $.top_level );
                $otherwise = Perlito::Java::LexicalBlock.new( block => $otherwise, needs_return => 1, top_level => $.top_level );
                $str = $str 
                    ~ 'if tobool( ' ~ Call::emit_java_call( $cond, 'Bool' ) ~ ' ) { ' 
                        ~ $body.emit_java ~ ' } else { ' 
                        ~ $otherwise.emit_java ~ ' }';
            }
            elsif $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                # Return, For - no changes for now 
                $str = $str ~ $last_statement.emit_java
            }
            else {
                $last_statement = Return.new( result => $last_statement );
                if $.top_level {
                    $str = $str ~ $last_statement.emit_java
                }
                else {
                    $str = $str ~ $last_statement.emit_java_simple
                }
            }
        }
        return $str;
    }
}

class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_java {
        my $class_name = Main::to_go_namespace($.name);
        my $str =
              '// instances of class ' ~ $.name ~ "\n"
            ~ 'type ' ~ $class_name ~ ' struct {' ~ "\n";
        for @((%.attributes).values) -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  ~ '  ' ~ 'v_' ~ ($decl.var).name ~ ' *Any;' ~ "\n"
            }
        }
        $str = $str ~ '}' ~ "\n";

        $str = $str 
            ~ '// methods in class ' ~ $.name ~ "\n"
            ~ 'var Method_' ~ $class_name ~ ' struct {' ~ "\n";
        for @((%.methods).values) -> $decl { 
            if $decl.isa( 'Method' ) {
                $str = $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' func (*' ~ $class_name ~ ', Capture) *Any;' ~ "\n";
            }
        }
        for @((%.attributes).values) -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  ~ '  ' ~ 'f_' ~ ($decl.var).name ~ ' func (*' ~ $class_name ~ ', Capture) *Any;' ~ "\n";
            }
        }
        $str = $str ~ '}' ~ "\n";

        $str = $str 
            ~ '// namespace ' ~ $.name ~ "\n"
            ~ 'var Namespace_' ~ $class_name ~ ' struct {' ~ "\n";
        for @.body -> $decl { 
            if $decl.isa( 'Sub' ) {
                $str = $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' Function;' ~ "\n";
            }
        }
        $str = $str ~ '}' ~ "\n";
        $str = $str ~ 'var Run_' ~ $class_name ~ ' func ();' ~ "\n";

        $str = $str 
            ~ '// method wrappers for ' ~ $.name ~ "\n";
        for @((%.methods).values) -> $decl { 
            if $decl.isa( 'Method' ) {
                $str = $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ $decl.name ~ ' (v Capture) *Any {' ~ "\n"
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ $decl.name ~ '(v_self, v);'~ "\n"
                    ~ '}'~ "\n";
            }
        }
        for @((%.attributes).values) -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ ($decl.var).name ~ ' (v Capture) *Any {' ~ "\n"
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name ~ '(v_self, v);'~ "\n"
                    ~ '}'~ "\n";
            }
        }
        if !( (%.methods){'isa'} ) {
            $str = $str ~ 'func (v_self *' ~ $class_name ~ ') f_isa (v Capture) *Any { '
                    ~ 'return toBool( "' ~ $.name ~ '" == tostr( v.p[0] ) ) '
                ~ '}' ~ "\n";
        }
        if !( (%.methods){'perl'} ) {
            $str = $str ~ 'func (v_self *' ~ $class_name ~ ') f_perl (v Capture) *Any { '
                    ~ 'return toStr( "::' ~ $.name ~ '(" ';
            my $sep = '';
            for @((%.attributes).values) -> $decl { 
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    $str = $str 
                        ~ $sep 
                        ~ '+ "' ~ ($decl.var).name ~ ' => "' 
                        ~ '+ tostr((*(*v_self).f_' ~ ($decl.var).name ~ '(Capture{})).(perl_er).f_perl(Capture{})) ';
                    $sep = '+ ", " ';
                }
            }
            $str = $str ~ '+ ")" ) }' ~ "\n";
        }
        if     (!( (%.methods){'Bool'} ))
            && (!( (%.attributes){'Bool'} )) 
        {
            $str = $str ~ 'func (v_self *' ~ $class_name ~ ') f_Bool (v Capture) *Any { '
                    ~ 'return b_true() '
                ~ '}' ~ "\n";
        }

        $str = $str 
            ~ '// prototype of ' ~ $.name ~ "\n"
            ~ 'var Proto_' ~ $class_name ~ ' *Any;' ~ "\n";

        $str = $str ~ 'func Init_' ~ $class_name ~ '() {' ~ "\n";

        $str = $str 
            ~ '  this_namespace := &Namespace_' ~ $class_name ~ ';' ~ "\n"
            ~ '  this_namespace = this_namespace;' ~ "\n";
        $str = $str  
            ~ '  Proto_' ~ $class_name ~ ' = '
                ~ 'func() *Any { '
                    ~ 'var m = new(' ~ $class_name ~ '); '
                    ~ 'var m1 Any = m; '
                    ~ 'return &m1; '
                ~ '}();' ~ "\n";

        my %decl_seen;
        for @.body -> $decl1 { 
            my $decl = $decl1;
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $decl = $decl.parameters;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                my $var_name = (($decl).var).emit_java;
                if !(%decl_seen{ $var_name }) {
                    $str = $str ~ $decl.emit_java_init;
                    %decl_seen{ $var_name } = 1;
                }

            }
        }

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str = $str  
              ~ '  // accessor ' ~ ($decl.var).name ~ "\n"
              ~ '  Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name 
                    ~ ' = func (v_self *' ~ $class_name ~ ', v Capture) *Any {' ~ "\n";

                $str = $str  
              ~ '    ' ~ 'if v_self.v_' ~ ($decl.var).name ~ ' == nil {' ~ "\n"
              ~ '      ' ~ (Decl.new(  
                                decl => 'my',
                                type => undef,
                                var => Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' ),
                            )).emit_java_init
              ~ '      ' ~ 'v_self.v_' ~ ($decl.var).name ~ ' = ' 
                                ~ (Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' )).emit_java ~ ';' ~ "\n"
              ~ '    ' ~ '}' ~ "\n";

                $str = $str  
              ~ '    ' ~ 'if *v_self.v_' ~ ($decl.var).name ~ ' == nil {' ~ "\n"
              ~ '      ' ~ (Decl.new(  
                                decl => 'my',
                                type => undef,
                                var => Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' ),
                            )).emit_java_init
              ~ '      ' ~ 'v_self.v_' ~ ($decl.var).name ~ ' = ' 
                                ~ (Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' )).emit_java ~ ';' ~ "\n"
              ~ '    ' ~ '}' ~ "\n";

                $str = $str  
              ~ '    ' ~ 'return v_self.v_' ~ ($decl.var).name ~ "\n"
              ~ '  };' ~ "\n";
            }
            if $decl.isa( 'Method' ) {
                my $sig      = $decl.sig;
                my $block    = Perlito::Java::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // method ' ~ $decl.name ~ "\n"
              ~ '  Method_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = func (self *' ~ $class_name ~ ', v Capture) *Any {' ~ "\n";
                $str = $str  
              ~ '    var self1 Any = self;' ~ "\n"
              ~ '    var ' ~ ($sig.invocant).emit_java ~ ' *Any = &self1;' ~ "\n"
              ~ '    ' ~ ($sig.invocant).emit_java ~ ' = ' ~ ($sig.invocant).emit_java ~ ';' ~ "\n"
              ~ '    ' ~ $sig.emit_java_bind ~ "\n";
                $str = $str  
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ $block.emit_java ~ '; p <- nil }(); ' ~ "\n"
              ~ '    return <-p; ' ~ "\n"
              ~ '  };' ~ "\n"
            }
            if $decl.isa( 'Sub' ) {
                my $sig      = $decl.sig;
                my $block    = Perlito::Java::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str = $str 
              ~ '  // sub ' ~ $decl.name ~ "\n"
              ~ '  Namespace_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = Function( func (v Capture) *Any {' ~ "\n";
                $str = $str  
              ~ '    ' ~ $sig.emit_java_bind ~ "\n"
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ $block.emit_java ~ '; p <- nil }(); ' ~ "\n";
                $str = $str  
              ~ '    return <-p; ' ~ "\n"
              ~ '  } );' ~ "\n";
            }
        }; 

        $str = $str 
            ~ '  // main runtime block of ' ~ $.name ~ "\n"
            ~ '  Run_' ~ $class_name ~ ' = func () {' ~ "\n";
        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str = $str ~ '    ' ~ ($decl).emit_java ~ ';' ~ "\n";
            }
        }; 
        $str = $str ~ '  }' ~ "\n";

        $str = $str ~ '}' ~ "\n";
        return $str;
    }

    sub emit_java_program( $comp_units ) {
        my $str = '';

        # join classes that have the same name
        # if there are method or accessor collisions, classes declared later have higher priority
        
        my %unit_seen;
        my @tmp_comp_unit;
        for @($comp_units) -> $comp_unit {
            my $name = $comp_unit.name;
            if %unit_seen{$name} {
                for @( $comp_unit.body ) -> $stmt {
                    push (%unit_seen{$name}).body, $stmt;
                }
            }
            else {
                %unit_seen{$name} = $comp_unit;
                push @tmp_comp_unit, $comp_unit;
            }
        }
        $comp_units = @tmp_comp_unit;

        for @($comp_units) -> $comp_unit {
            for @( $comp_unit.body ) -> $stmt {
                if $stmt.isa('Method') {
                    ($comp_unit.methods){ $stmt.name } = $stmt;
                }
                if $stmt.isa('Decl') && ( $stmt.decl eq 'has' ) {
                    ($comp_unit.attributes){ ($stmt.var).name } = $stmt;
                }
            }
        }
        
        # emit the code for all classes

        for @($comp_units) -> $comp_unit {
            $str = $str ~ $comp_unit.emit_java;
        }
        if !(%unit_seen{"Perlito::Grammar"}) {
            $str = $str ~ "type Perlito__Grammar struct\{}\n";
        }
        $str = $str ~ "// interfaces for all methods\n";
        my %meth_seen = (
            join => 1,
            perl => 1,
            scalar => 1,
            isa => 1,
            values => 1,
            keys => 1,
            exists => 1,
            bind => 1,
            int => 1,
            num => 1,
            str => 1,
            Str => 1,
            bool => 1,
            Bool => 1,
            array => 1,
            hash => 1,
            push => 1,
            pop => 1,
            shift => 1,
            lookup => 1,
            index => 1,
            function => 1,
        );
        for @($comp_units) -> $comp_unit {
            for @( $comp_unit.body ) -> $stmt {
                if $stmt.isa('Method') && !(%meth_seen{ $stmt.name }) {
                    my $meth = $stmt.name;
                    $str = $str ~ "type "
                          ~ $meth
                          ~ "_er interface \{ f_"
                          ~ $meth
                          ~ " (Capture) *Any }\n";
                    %meth_seen{$meth} = 1;
                }
                if $stmt.isa('Decl') && ( $stmt.decl eq 'has' ) && !(%meth_seen{ ($stmt.var).name }) {
                    my $meth = ($stmt.var).name;
                    $str = $str ~ "type "
                          ~ $meth
                          ~ "_er interface \{ f_"
                          ~ $meth
                          ~ " (Capture) *Any }\n";
                    %meth_seen{$meth} = 1;
                }
            }
        }

        $str = $str ~ "\n"
            ~ "func main () \{\n"
            ~ "  Init_Perlito__Match();\n";
        for @($comp_units) -> $comp_unit {
            $str = $str ~ "  Init_" ~ Main::to_go_namespace( $comp_unit.name ) ~ "();\n"
        }
        $str = $str ~ "  Init_Prelude();\n";
        for @($comp_units) -> $comp_unit {
            $str = $str ~ "  Run_" ~ Main::to_go_namespace( $comp_unit.name ) ~ "();\n"
        }
        $str = $str ~ '}' ~ "\n";
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit_java { 'new PerlitoInt(' ~ $.int ~ ')' }
}

class Val::Bit {
    has $.bit;
    method emit_java { 
        $.bit ?? 'new PerlitoBool(true)' !! 'new PerlitoBool(false)'
    }
}

class Val::Num {
    has $.num;
    method emit_java { 'new PerlitoNum(' ~ $.num ~ ')' }
}

class Val::Buf {
    has $.buf;
    method emit_java { 'new PerlitoString("' ~ $.buf ~ '")' }
}

class Val::Undef {
    method emit_java { 'u_undef()' }
}

class Lit::Array {
    has @.array1;
    method emit_java {
        my $ast = self.expand_interpolation;
        return $ast.emit_java;
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_java {
        my $ast = self.expand_interpolation;
        return $ast.emit_java;
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_java {
        '(*(*' ~ $.obj.emit_java ~ ').(array_er).f_array(Capture{}))' 
        ~ '.(index_er).f_index( Capture{ p : []*Any{ ' ~ $.index_exp.emit_java ~ ' }} )';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_java {
        '(*(*' ~ $.obj.emit_java ~ ').(hash_er).f_hash(Capture{}))' 
        ~ '.(lookup_er).f_lookup( Capture{ p : []*Any{ ' ~ $.index_exp.emit_java ~ ' }} )';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_java {
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
            $ns = Main::to_go_namespace($.namespace) ~ '.';
        }
           ( $.twigil eq '.' )
        ?? ( '(*v_self).(' ~ $.name ~ '_er).f_' ~ $.name ~ '(Capture{})' )
        !!  (    ( $.name eq '/' )
            ??   ( 'v_MATCH' )
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
    method emit_java {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] = [1, [2, 3]]
            
            my $a = $.parameters.array1;
            #my $b = $.arguments.array1;
            my $str = 
                'func () *Any { '
                    ~ 'List_tmp := ' ~ $.arguments.emit_java ~ '; ';
            my $i = 0;
            for @$a -> $var { 
                my $bind = Bind.new( 
                    parameters => $var, 
                    arguments  => Index.new(
                        obj        => Var.new( sigil => '@', twigil => '', namespace => '', name => 'tmp' ),
                        index_exp  => Val::Int.new( int => $i )
                    )
                );
                $str = $str ~ ' ' ~ $bind.emit_java ~ '; ';
                $i = $i + 1;
            };
            return $str ~ ' return List_tmp }()';
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
                    if ($var2[0]).buf eq ($var[0]).buf() {
                        $arg = $var2[1];
                    }
                };

                my $bind = Bind.new( parameters => $var[1], arguments => $arg );
                $str = $str ~ ' ' ~ $bind.emit_java ~ '; ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_java ~ ' }';
        };

        if $.parameters.isa( 'Call' ) {
            # $var.attr = 3;
            return 
                'func () *Any { ' 
                    ~ 'var tmp = ' ~ Call::emit_java_call( $.parameters.invocant, $.parameters.method ) ~ '; '
                    ~ '*tmp = *( ' ~ $.arguments.emit_java ~ ' ); '
                    ~ 'return tmp; '
                ~ '}()';
        }

        '*' ~ $.parameters.emit_java ~ ' = *(' ~ $.arguments.emit_java ~ ')';
    }
}

class Proto {
    has $.name;
    method emit_java {
        Main::to_go_namespace($.name)        
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    #has $.hyper;
    method emit_java {
        my $invocant = $.invocant.emit_java;
        if ($.invocant).isa( 'Proto' ) {
            if $.invocant.name eq 'self' {
                $invocant = 'v_self'
            }
            else {
                $invocant = 'Proto_' ~ $invocant
            }
        }

        my $meth = $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
            if ($.hyper) {
                $meth = '';  # ???
            }
            else {
                # ((*v_a).(function_er).f_function(Capture{}))
                return '((*' ~ $invocant ~ ').(function_er).f_function( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_java).join(', ') ~ ' } } ))';
            }
        };
        
        if ($.hyper) {
            return
                  'func (a_ *Any) *Any { ' ~ "\n"
                ~ '  var out = a_array(); ' ~ "\n"
                ~ '  var i = (*(*a_).(array_er).f_array(Capture{})).(*Array); ' ~ "\n"
                ~ '  for pos := 0; pos <= i.n; pos++ { ' ~ "\n"
                ~ '    (*out).(push_er).f_push( Capture{p: []*Any{ (*i.v[pos]).(' ~ $meth ~ '_er).f_' ~ $meth ~ '(Capture{ p : []*Any{}  }) }} )' ~ "\n"
                ~ '  } ' ~ "\n"
                ~ '  return out; ' ~ "\n"
                ~ '}(' ~ $invocant ~ ')'
        }
        return
            '(*' ~ $invocant ~ ').(' ~ $meth ~ '_er).f_' ~ $meth 
                    ~ '( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_java).join(', ') ~ ' } } )';

    }
    
    sub emit_java_call ( $invocant, $meth_name ) {
        my $invocant1 = $invocant.emit_java;
        if ($invocant).isa( 'Proto' ) {
            $invocant1 = 'Proto_' ~ $invocant1
        }
        my $meth = $meth_name;
        return
            '(*' ~ $invocant1 ~ ').(' ~ $meth ~ '_er).f_' ~ $meth ~ '(Capture{})';
    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_java {
        my $code = $.code;

        if $code.isa( 'Str' ) { }
        else {
            return '(' ~ $.code.emit_java ~ ')->(' ~ (@.arguments.>>emit).join(', ') ~ ')';
        };

        if $code eq 'self'       { return 'v_self' };
        if $code eq 'False'      { return 'b_false()' };
        if $code eq 'True'       { return 'b_true()' };
        if $code eq 'make'       { 
            return 
                  'func () *Any { ' 
                    ~ 'tmp := ' ~ (@.arguments.>>emit_java).join(', ') ~ '; '
                    ~ '*(*v_MATCH).(capture_er).f_capture(Capture{}) = *tmp; '
                    ~ 'return tmp; '
                ~ '}()';
        };

        if $code eq 'go'            { 
                                      return 'go func () { ' ~ "\n"
                                                ~ '        ' ~ (Perlito::Java::LexicalBlock.new( block => (@.arguments[0]).block, needs_return => 0, top_level => 1 )).emit_java ~ "\n"
                                                ~ '    }()'
                                    }

        if $code eq 'say'           { return 'System.out.println( '    
                                                ~ (@.arguments.>>emit_java).join('.to_string() + ') 
                                                ~ '.to_string() )'
                                    }
        if $code eq 'print'         { return 'System.out.print( '  
                                                ~ (@.arguments.>>emit_java).join('.to_string() ') 
                                                ~ '.to_string() )' 
                                    }
        if $code eq 'warn'          { return 'f_print_stderr( Capture{ p : []*Any{ '   
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ', toStr("\n") } } )' 
                                    }
        if $code eq 'die'           { return 'f_die( Capture{ p : []*Any{ '   
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'defined'       { return 'f_defined( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'pop'           { return 'f_pop( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'push'          { return 'f_push( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'shift'         { return 'f_shift( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'index'         { return 'f_index( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'substr'        { return 'f_substr( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_java).join(', ') 
                                                ~ ' } } )'  
                                    }
        if $code eq 'scalar'        { return 'f_scalar( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_java).join(', ')    
                                                ~ ' } } )' 
                                    }
        if $code eq 'Int'           { return 'toInt(' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_java ~ ')'
                                        ~ ')' 
                                    };
        if $code eq 'Num'           { return 'toNum('   
                                        ~ 'tonum(' ~ (@.arguments[0]).emit_java ~ ')'
                                        ~ ')' 
                                    };
        if $code eq 'exists'        { 
                                      my $arg = @.arguments[0];
                                      if $arg.isa( 'Lookup' ) {
                                        return '(*' ~ ($arg.obj).emit_java ~ ')' 
                                            ~ '.(exists_er).f_exists(Capture{ p : []*Any{ ' 
                                            ~ ($arg.index_exp).emit_java 
                                            ~ ' } } )';
                                      }
                                    }
        if $code eq 'prefix:<~>'    { return Call::emit_java_call( @.arguments[0], 'Str' ) }
        if $code eq 'prefix:<!>'    { return 'toBool(!tobool(' ~ ( @.arguments[0]).emit_java ~ '))' };
        if $code eq 'prefix:<?>'    { return Call::emit_java_call( @.arguments[0], 'Bool') } 
        if $code eq 'prefix:<$>'    { return 'f_scalar( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_java).join(', ')    
                                                ~ ' } } )' 
                                    }
        if $code eq 'prefix:<@>'    { return Call::emit_java_call( @.arguments[0], 'array' ) }
        if $code eq 'prefix:<%>'    { return Call::emit_java_call( @.arguments[0], 'hash' ) }

        if $code eq 'infix:<~>'     { return 'toStr( ' 
                                                ~ 'tostr(' ~ (@.arguments[0]).emit_java ~ ') + ' 
                                                ~ 'tostr(' ~ (@.arguments[1]).emit_java ~ ') ' 
                                                ~ ')' 
                                    }
        if $code eq 'infix:<+>'     { return 'f_add( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:<->'     { return 'f_sub( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:<*>'     { return 'f_mul( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:</>'     { return 'f_div( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:<>>'     { return 'f_greater( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:<<>'     { return 'f_smaller( ' 
                                                ~ (@.arguments.>>emit_java).join(', ') ~ ')' }
        if $code eq 'infix:<>=>'     { return 'toBool( ' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_java ~ ') >= '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_java ~ ') '
                                        ~ ')' 
                                    }
        if $code eq 'infix:<<=>'     { return 'toBool( ' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_java ~ ') <= '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_java ~ ') '
                                        ~ ')' 
                                    }

        if $code eq 'infix:<&&>' { 
            return 
                'f_and( '
                    ~ 'func () *Any { return ' ~ (@.arguments[0]).emit_java ~ ' }, ' 
                    ~ 'func () *Any { return ' ~ (@.arguments[1]).emit_java ~ ' } ' 
                ~ ')'
        };

        if $code eq 'infix:<||>' { 
            return 
                'f_or( '
                    ~ 'func () *Any { return ' ~ (@.arguments[0]).emit_java ~ ' }, ' 
                    ~ 'func () *Any { return ' ~ (@.arguments[1]).emit_java ~ ' } ' 
                ~ ')'
        };

        if $code eq 'infix:<eq>'    { return 'toBool(' 
                                        ~ 'tostr(' ~ (@.arguments[0]).emit_java ~ ') == '
                                        ~ 'tostr(' ~ (@.arguments[1]).emit_java ~ ')' 
                                        ~ ')' 
                                    };
        if $code eq 'infix:<ne>'    { return 'toBool(' 
                                        ~ 'tostr(' ~ (@.arguments[0]).emit_java ~ ') != '
                                        ~ 'tostr(' ~ (@.arguments[1]).emit_java ~ ')' 
                                        ~ ')' 
                                    };
 
        if $code eq 'infix:<==>' { return 'toBool(' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_java ~ ') == '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_java ~ ') '
                                    ~ ')' 
                                };
        if $code eq 'infix:<!=>' { return 'toBool(' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_java ~ ') != '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_java ~ ') '
                                    ~ ')' 
                                };

        if $code eq 'ternary:<?? !!>' { 
            return 
                'func () *Any { '
                    ~ 'if tobool( ' ~ Call::emit_java_call( @.arguments[0], 'Bool' ) ~ ' ) ' 
                    ~ '{ return ' ~ (@.arguments[1]).emit_java ~ ' }; '
                    ~ 'return ' ~ (@.arguments[2]).emit_java ~ ' '
                ~ '}()'
        };
        
        $code = 'f_' ~ $.code;
        if $.namespace {
            $code = 'Namespace_' ~ Main::to_go_namespace($.namespace) ~ '.' ~ $code;
        }
        else {
            $code = 'this_namespace.' ~ $code;
        }
        $code ~ '( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_java).join(', ') ~ ' } } )';
    }
}

class Return {
    has $.result;
    method emit_java {
        if ($.result).isa( 'Bind' ) {
            my $tmp = ($.result).parameters;
            return 
                '(func () { '
                    ~ ($.result).emit_java ~ '; ' 
                    ~ 'p <- ' ~ $tmp.emit_java ~ '; '
                    ~ 'runtime.Javaexit(); '
                ~ '}())'
        }
        return
                '(func () { '
                    ~ 'var tmp *Any = ' ~ ($.result).emit_java ~ '; '
                    ~ 'p <- tmp; '
                    ~ 'runtime.Javaexit(); '
                ~ '}())'
    }
    method emit_java_simple {
        if ($.result).isa( 'Bind' ) {
            my $tmp = ($.result).parameters;
            return 
                'return (func () *Any { '
                    ~ ($.result).emit_java ~ '; ' 
                    ~ 'return ' ~ $tmp.emit_java ~ '; '
                ~ '}())'
        }
        return
            'return( ' ~ $.result.emit_java ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_java {
        my $cond = $.cond;

        if   $cond.isa( 'Apply' ) 
          && $cond.code eq 'prefix:<!>' 
        {
            my $if = If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.emit_java;
        }
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        my $s = 'if tobool( ' ~ Call::emit_java_call( $cond, 'Bool' ) ~ ' ) { ' 
                    ~ (Perlito::Java::LexicalBlock.new( block => @.body, needs_return => 0 )).emit_java 
                ~ ' }';
        if !(@.otherwise) {
            return $s;
        }
        return $s 
                ~ ' else { ' 
                    ~ (Perlito::Java::LexicalBlock.new( block => @.otherwise, needs_return => 0 )).emit_java 
                ~ ' }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_java {
          'func (a_ *Any) { ' ~ "\n"
        ~ '  var i = (*(*a_).(array_er).f_array(Capture{})).(*Array); ' ~ "\n"
        ~ '  for pos := 0; pos <= i.n; pos++ { ' ~ "\n"
        ~ '    func (' ~ $.topic.emit_java ~ ' *Any) { ' ~ "\n"
        ~ '      ' ~ (Perlito::Java::LexicalBlock.new( block => @.body, needs_return => 0 )).emit_java ~ "\n"
        ~ '    }(i.v[pos]) ' ~ "\n"
        ~ '  } ' ~ "\n"
        ~ '}(' ~ $.cond.emit_java ~ ')'
    }
}

class When {
    has @.parameters;
    has @.body;
    method emit_java { die "TODO - When" }
}

class While {
    has $.init;
    has $.cond;
    has $.continue;
    has @.body;
    method emit_java { 
        my $cond = $.cond;
        if   $cond.isa( 'Var' )
          && $cond.sigil eq '@'
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        return  'for ' 
             ~    ( $.init ?? $.init.emit_java ~ '; ' !! ';' )
             ~    'tobool( ' ~ Call::emit_java_call( $cond, 'Bool' ) ~ ' ); '  
             ~    ( $.continue ?? $.continue.emit_java ~ ' ' !! '' )
             ~  '{ '
             ~      (Perlito::Java::LexicalBlock.new( block => @.body, needs_return => 0 )).emit_java
             ~ ' }';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_java {
        $.var.emit_java;
    }
    method emit_java_init {
        if $.decl eq 'my' {
            my $str = "";
            $str = $str ~ 'var ' ~ ($.var).emit_java ~ ' *Any;' ~ "\n";
            $str = $str ~ ($.var).emit_java ~ ' = ' ~ ($.var).emit_java ~ ';' ~ "\n";
            if ($.var).sigil eq '%' {
                $str = $str ~ ($.var).emit_java ~ ' = h_hash();' ~ "\n";
            }
            elsif ($.var).sigil eq '@' {
                $str = $str ~ ($.var).emit_java ~ ' = a_array();' ~ "\n";
            }
            else {
                $str = $str ~ ($.var).emit_java ~ ' = u_undef();' ~ "\n";
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
    method emit_java {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    }
    method emit_java_bind {
        my $str = '';
        my $i = 0;
        for @($.positional) -> $decl { 
            $str = $str  
                ~ "  var " ~ $decl.emit_java ~ " *Any;\n"
                ~ "  if len(v.p) > " ~ $i ~ " \{\n"
                ~ "    " ~ $decl.emit_java ~ " = v.p[" ~ $i ~ "];\n"
                ~ "  }\n";
            # avoid "x declared and not used"
            $str = $str ~ $decl.emit_java ~ ' = ' ~ $decl.emit_java ~ '; ';
            $i = $i + 1;
        }
        return $str;
    }
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_java {
        my $invocant = ($.sig).invocant; 
        'func ' ~ $.name ~ '(v Capture) *Any { ' 
              ~ '    ' ~ ($.sig).emit_java_bind ~ "\n"
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ (Perlito::Java::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_java 
              ~ '; p <- nil }(); ' ~ "\n"
              ~ '    return <-p; ' ~ "\n"
        ~ ' }'
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_java {
        if $.name eq '' { 
            return
                'toFunction( func(v Capture) *Any { '
                    ~ '    ' ~ ($.sig).emit_java_bind ~ "\n"
                    ~ '    p := make(chan *Any); go func () { ' ~ "\n"
                    ~ '        ' ~ (Perlito::Java::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_java 
                    ~ '; p <- nil }(); ' ~ "\n"
                    ~ '    return <-p; ' ~ "\n"
                    ~ '} '
                ~ ')'
        }

        'func ' ~ $.name ~ '(v Capture) *Any { ' 
                    ~ '    ' ~ ($.sig).emit_java_bind ~ "\n"
                    ~ '    p := make(chan *Any); go func () { ' ~ "\n"
                    ~ '        ' ~ (Perlito::Java::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_java 
                    ~ '; p <- nil }(); ' ~ "\n"
                    ~ '    return <-p; ' ~ "\n"
        ~ ' }'
    }
}

class Do {
    has @.block;
    method emit_java {
        '(func () *Any { ' 
          ~ (Perlito::Java::LexicalBlock.new( block => @.block, needs_return => 1 )).emit_java 
          ~ '; return u_undef() '
        ~ '})()'
    }
}

class Use {
    has $.mod;
    method emit_java {
        '// use ' ~ $.mod ~ "\n"
    }
}

=begin

=head1 NAME 

Perlito::Java::Emit - Code generator for Perlito-in-Java

=head1 SYNOPSIS

    $program.emit_java  # generated Java code

=head1 DESCRIPTION

This module generates Java code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2011 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

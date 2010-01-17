use v6;

class MiniPerl6::Go::LexicalBlock {
    has @.block;
    has $.needs_return;
    has $.top_level;
    method emit_go {
        if !(@.block) {
            return '';
        }
        my $str := '';
        my %decl_seen;
        for @.block -> $decl1 { 
            my $decl := $decl1;
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $decl := $decl.parameters;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                my $var_name := (($decl).var).emit_go;
                if !(%decl_seen{ $var_name }) {
                    $str := $str ~ $decl.emit_go_init;
                    %decl_seen{ $var_name } := 1;
                }
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
                    $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
                };
                $body      := MiniPerl6::Go::LexicalBlock.new( block => $body, needs_return => 1, top_level => $.top_level );
                $otherwise := MiniPerl6::Go::LexicalBlock.new( block => $otherwise, needs_return => 1, top_level => $.top_level );
                $str := $str 
                    ~ 'if tobool( ' ~ Call::emit_go_call( $cond, 'Bool' ) ~ ' ) { ' 
                        ~ $body.emit_go ~ ' } else { ' 
                        ~ $otherwise.emit_go ~ ' }';
            }
            else {
            if $last_statement.isa( 'Return' ) || $last_statement.isa( 'For' ) {
                # Return, For - no changes for now 
                $str := $str ~ $last_statement.emit_go
            }
            else {
                $last_statement := Return.new( result => $last_statement );
                if $.top_level {
                    $str := $str ~ $last_statement.emit_go
                }
                else {
                    $str := $str ~ $last_statement.emit_go_simple
                }
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
    method emit_go {
        my $class_name := Main::to_go_namespace($.name);
        my $str :=
              '// instances of class ' ~ $.name ~ "\n"
            ~ 'type ' ~ $class_name ~ ' struct {' ~ "\n";
        for (%.attributes).values -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  ~ '  ' ~ 'v_' ~ ($decl.var).name ~ ' *Any;' ~ "\n"
            }
        }
        $str := $str ~ '}' ~ "\n";

        $str := $str 
            ~ '// methods in class ' ~ $.name ~ "\n"
            ~ 'var Method_' ~ $class_name ~ ' struct {' ~ "\n";
        for (%.methods).values -> $decl { 
            if $decl.isa( 'Method' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' func (*' ~ $class_name ~ ', Capture) *Any;' ~ "\n";
            }
        }
        for (%.attributes).values -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ ($decl.var).name ~ ' func (*' ~ $class_name ~ ', Capture) *Any;' ~ "\n";
            }
        }
        $str := $str ~ '}' ~ "\n";

        $str := $str 
            ~ '// namespace ' ~ $.name ~ "\n"
            ~ 'var Namespace_' ~ $class_name ~ ' struct {' ~ "\n";
        for @.body -> $decl { 
            if $decl.isa( 'Sub' ) {
                $str := $str  ~ '  ' ~ 'f_' ~ $decl.name ~ ' Function;' ~ "\n";
            }
        }
        $str := $str ~ '}' ~ "\n";
        $str := $str ~ 'var Run_' ~ $class_name ~ ' func ();' ~ "\n";

        $str := $str 
            ~ '// method wrappers for ' ~ $.name ~ "\n";
        for (%.methods).values -> $decl { 
            if $decl.isa( 'Method' ) {
                $str := $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ $decl.name ~ ' (v Capture) *Any {' ~ "\n"
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ $decl.name ~ '(v_self, v);'~ "\n"
                    ~ '}'~ "\n";
            }
        }
        for (%.attributes).values -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  
                    ~ 'func (v_self *' ~ $class_name ~ ') f_' ~ ($decl.var).name ~ ' (v Capture) *Any {' ~ "\n"
                    ~ '  return Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name ~ '(v_self, v);'~ "\n"
                    ~ '}'~ "\n";
            }
        }
        if !( (%.methods){'isa'} ) {
            $str := $str ~ 'func (v_self *' ~ $class_name ~ ') f_isa (v Capture) *Any { '
                    ~ 'return toBool( "' ~ $.name ~ '" == tostr( v.p[0] ) ) '
                ~ '}' ~ "\n";
        }
        if !( (%.methods){'perl'} ) {
            $str := $str ~ 'func (v_self *' ~ $class_name ~ ') f_perl (v Capture) *Any { '
                    ~ 'return toStr( "::' ~ $.name ~ '(" ';
            my $sep := '';
            for (%.attributes).values -> $decl { 
                if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                    $str := $str 
                        ~ $sep 
                        ~ '+ "' ~ ($decl.var).name ~ ' => "' 
                        ~ '+ tostr((*(*v_self).f_' ~ ($decl.var).name ~ '(Capture{})).(perl_er).f_perl(Capture{})) ';
                    $sep := '+ ", " ';
                }
            }
            $str := $str ~ '+ ")" ) }' ~ "\n";
        }
        if     (!( (%.methods){'Bool'} ))
            && (!( (%.attributes){'Bool'} )) 
        {
            $str := $str ~ 'func (v_self *' ~ $class_name ~ ') f_Bool (v Capture) *Any { '
                    ~ 'return b_true() '
                ~ '}' ~ "\n";
        }

        $str := $str 
            ~ '// prototype of ' ~ $.name ~ "\n"
            ~ 'var Proto_' ~ $class_name ~ ' *Any;' ~ "\n";

        $str := $str ~ 'func Init_' ~ $class_name ~ '() {' ~ "\n";

        $str := $str 
            ~ '  this_namespace := &Namespace_' ~ $class_name ~ ';' ~ "\n"
            ~ '  this_namespace = this_namespace;' ~ "\n";
        $str := $str  
            ~ '  Proto_' ~ $class_name ~ ' = '
                ~ 'func() *Any { '
                    ~ 'var m = new(' ~ $class_name ~ '); '
                    ~ 'var m1 Any = m; '
                    ~ 'return &m1; '
                ~ '}();' ~ "\n";

        for @.body -> $decl1 { 
            my $decl := $decl1;
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $decl := $decl.parameters;
            }
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $str := $str ~ $decl.emit_go_init;
            }
        }

        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                $str := $str  
              ~ '  // accessor ' ~ ($decl.var).name ~ "\n"
              ~ '  Method_' ~ $class_name ~ '.f_' ~ ($decl.var).name 
                    ~ ' = func (v_self *' ~ $class_name ~ ', v Capture) *Any {' ~ "\n";

                $str := $str  
              ~ '    ' ~ 'if v_self.v_' ~ ($decl.var).name ~ ' == nil {' ~ "\n"
              ~ '      ' ~ (Decl.new(  
                                decl => 'my',
                                type => undef,
                                var => Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' ),
                            )).emit_go_init
              ~ '      ' ~ 'v_self.v_' ~ ($decl.var).name ~ ' = ' 
                                ~ (Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' )).emit_go ~ ';' ~ "\n"
              ~ '    ' ~ '}' ~ "\n";

                $str := $str  
              ~ '    ' ~ 'if *v_self.v_' ~ ($decl.var).name ~ ' == nil {' ~ "\n"
              ~ '      ' ~ (Decl.new(  
                                decl => 'my',
                                type => undef,
                                var => Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' ),
                            )).emit_go_init
              ~ '      ' ~ 'v_self.v_' ~ ($decl.var).name ~ ' = ' 
                                ~ (Var.new( sigil => ($decl.var).sigil, twigil => '', namespace => '', name => 'tmp' )).emit_go ~ ';' ~ "\n"
              ~ '    ' ~ '}' ~ "\n";

                $str := $str  
              ~ '    ' ~ 'return v_self.v_' ~ ($decl.var).name ~ "\n"
              ~ '  };' ~ "\n";
            }
            if $decl.isa( 'Method' ) {
                my $sig      := $decl.sig;
                my $block    := MiniPerl6::Go::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str := $str 
              ~ '  // method ' ~ $decl.name ~ "\n"
              ~ '  Method_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = func (self *' ~ $class_name ~ ', v Capture) *Any {' ~ "\n";
                $str := $str  
              ~ '    var self1 Any = self;' ~ "\n"
              ~ '    var ' ~ ($sig.invocant).emit_go ~ ' *Any = &self1;' ~ "\n"
              ~ '    ' ~ ($sig.invocant).emit_go ~ ' = ' ~ ($sig.invocant).emit_go ~ ';' ~ "\n"
              ~ '    ' ~ $sig.emit_go_bind ~ "\n";
                $str := $str  
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ $block.emit_go ~ '; return }(); ' ~ "\n"
              ~ '    return <-p; ' ~ "\n"
              ~ '  };' ~ "\n"
            }
            if $decl.isa( 'Sub' ) {
                my $sig      := $decl.sig;
                my $block    := MiniPerl6::Go::LexicalBlock.new( block => $decl.block, needs_return => 1, top_level => 1 );
                $str := $str 
              ~ '  // sub ' ~ $decl.name ~ "\n"
              ~ '  Namespace_' ~ $class_name ~ '.f_' ~ $decl.name 
                    ~ ' = Function( func (v Capture) *Any {' ~ "\n";
                $str := $str  
              ~ '    ' ~ $sig.emit_go_bind ~ "\n"
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ $block.emit_go ~ '; return }(); ' ~ "\n";
                $str := $str  
              ~ '    return <-p; ' ~ "\n"
              ~ '  } );' ~ "\n";
            }
        }; 

        $str := $str 
            ~ '  // main runtime block of ' ~ $.name ~ "\n"
            ~ '  Run_' ~ $class_name ~ ' = func () {' ~ "\n";
        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str := $str ~ '    ' ~ ($decl).emit_go ~ ';' ~ "\n";
            }
        }; 
        $str := $str ~ '  }' ~ "\n";

        $str := $str ~ '}' ~ "\n";
        return $str;
    }

    sub emit_go_program( $comp_units ) {
        my $str := '';

        # join classes that have the same name
        # if there are method or accessor collisions, classes declared later have higher priority
        
        my %unit_seen;
        my @tmp_comp_unit;
        for @($comp_units) -> $comp_unit {
            my $name := $comp_unit.name;
            if %unit_seen{$name} {
                for @( $comp_unit.body ) -> $stmt {
                    push (%unit_seen{$name}).body, $stmt;
                }
            }
            else {
                %unit_seen{$name} := $comp_unit;
                push @tmp_comp_unit, $comp_unit;
            }
        }

        $comp_units := @tmp_comp_unit;
        for @($comp_units) -> $comp_unit {
            for @( $comp_unit.body ) -> $stmt {
                if $stmt.isa('Method') {
                    ($comp_unit.methods){ $stmt.name } := $stmt;
                }
                if $stmt.isa('Decl') && ( $stmt.decl eq 'has' ) {
                    ($comp_unit.attributes){ ($stmt.var).name } := $stmt;
                }
            }
        }
        
        # emit the code for all classes

        for @($comp_units) -> $comp_unit {
            $str := $str ~ $comp_unit.emit_go;
        }
        if !(%unit_seen{"MiniPerl6::Grammar"}) {
            $str := $str ~ "type MiniPerl6__Grammar struct{}\n";
        }
        $str := $str ~ "// interfaces for all methods\n";
        my %meth_seen := {
            join => 1,
            perl => 1,
            scalar => 1,
            isa => 1,
            values => 1,
            keys => 1,
            bind => 1,
            int => 1,
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
        };
        for @($comp_units) -> $comp_unit {
            for @( $comp_unit.body ) -> $stmt {
                if $stmt.isa('Method') && !(%meth_seen{ $stmt.name }) {
                    my $meth := $stmt.name;
                    $str := $str ~ "type "
                          ~ $meth
                          ~ "_er interface { f_"
                          ~ $meth
                          ~ " (Capture) *Any }\n";
                    %meth_seen{$meth} := 1;
                }
                if $stmt.isa('Decl') && ( $stmt.decl eq 'has' ) && !(%meth_seen{ ($stmt.var).name }) {
                    my $meth := ($stmt.var).name;
                    $str := $str ~ "type "
                          ~ $meth
                          ~ "_er interface { f_"
                          ~ $meth
                          ~ " (Capture) *Any }\n";
                    %meth_seen{$meth} := 1;
                }
            }
        }

        $str := $str ~ "\n"
            ~ "func main () {\n"
            ~ "  Init_MiniPerl6__Match();\n";
        for @($comp_units) -> $comp_unit {
            $str := $str ~ "  Init_" ~ Main::to_go_namespace( $comp_unit.name ) ~ "();\n"
        }
        $str := $str ~ "  Init_Prelude();\n";
        for @($comp_units) -> $comp_unit {
            $str := $str ~ "  Run_" ~ Main::to_go_namespace( $comp_unit.name ) ~ "();\n"
        }
        $str := $str ~ '}' ~ "\n";
        return $str;
    }
}

class Val::Int {
    has $.int;
    method emit_go { 'toInt(' ~ $.int ~ ')' }
}

class Val::Bit {
    has $.bit;
    method emit_go { 'toBit(' ~ $.bit ~ ')' }
}

class Val::Num {
    has $.num;
    method emit_go { 'toNum(' ~ $.num ~ ')' }
}

class Val::Buf {
    has $.buf;
    method emit_go { 'toStr("' ~ Main::javascript_escape_string($.buf) ~ '")' }
}

class Val::Undef {
    method emit_go { 'u_undef()' }
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
        '[]*Any{ '
            ~ (@.seq.>>emit_go).join(', ') 
        ~ ' }';
    }
}

class Lit::Array {
    has @.array1;
    method emit_go {
        my $str := '';
        for @.array1 -> $item {
            if     ( $item.isa( 'Var' )   && $item.sigil eq '@' )
                || ( $item.isa( 'Apply' ) && $item.code  eq 'prefix:<@>' ) 
            {
                $str := $str 
                    ~ 'func(a_ *Array) { ' 
                        ~ 'for i_ := 0; i_ <= a_.n; i_++ { (*a).(push_er).f_push( Capture{ p: []*Any{ a_.v[i_] } } ) } ' 
                    ~ '}( (*' ~ Call::emit_go_call( $item, 'array' ) ~ ').(*Array) ); '
            }
            else {
                $str := $str ~ '(*a).(push_er).f_push( Capture{ p: []*Any{ ' ~ $item.emit_go ~ ' } } ); '
            }
        }
        'func () *Any { ' 
                ~ 'a := a_array(); '
                ~ $str 
                ~ 'return a; '
        ~ '}()'
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_go {
        my $fields := @.hash1;
        my $str := ''; 
        for @$fields -> $field { 
            $str := $str 
                ~ '*(*m).(lookup_er).f_lookup( Capture{ p : []*Any{ ' ~ ($field[0]).emit_go ~ ' }} ) = *(' ~ ($field[1]).emit_go ~ '); ';
        }; 
        'func() *Any { ' 
            ~ 'm := h_hash(); '
            ~ $str 
            ~ 'return m; '
        ~ '}()';
    }
}

class Lit::Code {
    # XXX
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_go {
        my $fields := @.fields;
        my $str := '';
        for @$fields -> $field { 
            $str := $str 

                ~ 'if m.v_' ~ ($field[0]).buf ~ ' == nil {' ~ "\n"
                ~     'var p Any; ' ~ "\n"
                ~     'm.v_' ~ ($field[0]).buf ~ ' = &p; ' ~ "\n"
                ~ '}' ~ "\n"

                ~ '*m.v_' ~ ($field[0]).buf ~ ' = *' ~ ($field[1]).emit_go ~ '; ' ~ "\n";
        }; 
          'func() *Any { ' ~ "\n"
        ~ '  var m = new(' ~ Main::to_go_namespace($.class) ~ '); ' ~ "\n"
        ~ '  ' ~ $str ~ "\n"
        ~ '  var m1 Any = m; ' ~ "\n"
        ~ '  return &m1; ' ~ "\n"
        ~ '}()';
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_go {
        '(*(*' ~ $.obj.emit_go ~ ').(array_er).f_array(Capture{}))' 
        ~ '.(index_er).f_index( Capture{ p : []*Any{ ' ~ $.index_exp.emit_go ~ ' }} )';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_go {
        '(*(*' ~ $.obj.emit_go ~ ').(hash_er).f_hash(Capture{}))' 
        ~ '.(lookup_er).f_lookup( Capture{ p : []*Any{ ' ~ $.index_exp.emit_go ~ ' }} )';
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
    method emit_go {
        if $.parameters.isa( 'Lit::Array' ) {
            
            #  [$a, [$b, $c]] := [1, [2, 3]]
            
            my $a := $.parameters.array1;
            #my $b := $.arguments.array1;
            my $str := 
                'func () *Any { '
                    ~ 'List_tmp := ' ~ $.arguments.emit_go ~ '; ';
            my $i := 0;
            for @$a -> $var { 
                my $bind := Bind.new( 
                    'parameters' => $var, 
                    'arguments'  => Index.new(
                        obj        => Var.new( sigil => '@', twigil => '', namespace => '', name => 'tmp' ),
                        index_exp  => Val::Int.new( int => $i )
                    )
                );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ ' return List_tmp }()';
        };
        if $.parameters.isa( 'Lit::Hash' ) {

            #  {:$a, :$b} := { a => 1, b => [2, 3]}

            my $a := $.parameters.hash1;
            my $b := $.arguments.hash1;
            my $str := 'do { ';
            my $i := 0;
            my $arg;
            for @$a -> $var {

                $arg := Val::Undef.new();
                for @$b -> $var2 {
                    #say "COMPARE ", ($var2[0]).buf, ' eq ', ($var[0]).buf;
                    if ($var2[0]).buf eq ($var[0]).buf {
                        $arg := $var2[1];
                    }
                };

                my $bind := Bind.new( 'parameters' => $var[1], 'arguments' => $arg );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit_go ~ ' }';
        };

        if $.parameters.isa( 'Lit::Object' ) {

            #  Obj.new(:$a, :$b) := $obj

            my $class := $.parameters.class;
            my $a     := $.parameters.fields;
            my $b     := $.arguments;
            my $str   := 'do { ';
            my $i     := 0;
            my $arg;
            for @$a -> $var {
                my $bind := Bind.new( 
                    'parameters' => $var[1], 
                    'arguments'  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.emit_go ~ '; ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit_go ~ ' }';
        };
    
        if $.parameters.isa( 'Call' ) {
            # $var.attr := 3;
            return 
                'func () *Any { ' 
                    ~ 'var tmp = ' ~ Call::emit_go_call( $.parameters.invocant, $.parameters.method ) ~ '; '
                    ~ '*tmp = *( ' ~ $.arguments.emit_go ~ ' ); '
                    ~ 'return tmp; '
                ~ '}()';
        }

        '*' ~ $.parameters.emit_go ~ ' = *(' ~ $.arguments.emit_go ~ ')';
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
            if $.invocant.name eq 'self' {
                $invocant := 'v_self'
            }
            else {
                $invocant := 'Proto_' ~ $invocant
            }
        }

        my $meth := $.method;
        if  $meth eq 'postcircumfix:<( )>'  {
            if ($.hyper) {
                $meth := '';  # ???
            }
            else {
                return $invocant ~ '( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
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
                    ~ '( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';

    }
    
    sub emit_go_call ( $invocant, $meth_name ) {
        my $invocant1 := $invocant.emit_go;
        if ($invocant).isa( 'Proto' ) {
            $invocant1 := 'Proto_' ~ $invocant1
        }
        my $meth := $meth_name;
        return
            '(*' ~ $invocant1 ~ ').(' ~ $meth ~ '_er).f_' ~ $meth ~ '(Capture{})';
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
        if $code eq 'false'      { return 'b_false()' };
        if $code eq 'make'       { 
            return 
                  'func () *Any { ' 
                    ~ 'tmp := ' ~ (@.arguments.>>emit_go).join(', ') ~ '; '
                    ~ '*(*v_MATCH).(capture_er).f_capture(Capture{}) = *tmp; '
                    ~ 'return tmp; '
                ~ '}()';
        };

        if $code eq 'say'           { return 'f_print( Capture{ p : []*Any{ '    
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ', toStr("\n") } } )'
                                    }
        if $code eq 'print'         { return 'f_print( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'warn'          { return 'f_print_stderr( Capture{ p : []*Any{ '   
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ', toStr("\n") } } )' 
                                    }
        if $code eq 'die'           { return 'f_die( Capture{ p : []*Any{ '   
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'defined'       { return 'f_defined( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'pop'           { return 'f_pop( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'push'          { return 'f_push( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'shift'         { return 'f_shift( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'index'         { return 'f_index( Capture{ p : []*Any{ '  
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )' 
                                    }
        if $code eq 'substr'        { return 'f_substr( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_go).join(', ') 
                                                ~ ' } } )'  
                                    }
        if $code eq 'scalar'        { return 'f_scalar( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_go).join(', ')    
                                                ~ ' } } )' 
                                    }
        if $code eq 'prefix:<~>'    { return Call::emit_go_call( @.arguments[0], 'Str' ) }
        if $code eq 'prefix:<!>'    { return 'toBool(!tobool(' ~ ( @.arguments[0]).emit_go ~ '))' };
        if $code eq 'prefix:<?>'    { return Call::emit_go_call( @.arguments[0], 'Bool') } 
        if $code eq 'prefix:<$>'    { return 'f_scalar( Capture{ p : []*Any{ ' 
                                                ~ (@.arguments.>>emit_go).join(', ')    
                                                ~ ' } } )' 
                                    }
        if $code eq 'prefix:<@>'    { return Call::emit_go_call( @.arguments[0], 'array' ) }
        if $code eq 'prefix:<%>'    { return Call::emit_go_call( @.arguments[0], 'hash' ) }

        if $code eq 'infix:<~>'     { return 'toStr( ' 
                                                ~ 'tostr(' ~ (@.arguments[0]).emit_go ~ ') + ' 
                                                ~ 'tostr(' ~ (@.arguments[1]).emit_go ~ ') ' 
                                                ~ ')' 
                                    };
        if $code eq 'infix:<+>'     { return 'toInt( ' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_go ~ ') + '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_go ~ ') ' 
                                        ~ ')' 
                                    };
        if $code eq 'infix:<->'     { return 'toInt( ' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_go ~ ') - '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_go ~ ') '
                                        ~ ')' 
                                    };
        if $code eq 'infix:<>>'     { return 'toBool( ' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_go ~ ') > '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_go ~ ') '
                                        ~ ')' 
                                    };

        if $code eq 'infix:<&&>' { 
            return 
                'f_and( '
                    ~ 'func () *Any { return ' ~ (@.arguments[0]).emit_go ~ ' }, ' 
                    ~ 'func () *Any { return ' ~ (@.arguments[1]).emit_go ~ ' } ' 
                ~ ')'
        };

        if $code eq 'infix:<||>' { 
            return 
                'f_or( '
                    ~ 'func () *Any { return ' ~ (@.arguments[0]).emit_go ~ ' }, ' 
                    ~ 'func () *Any { return ' ~ (@.arguments[1]).emit_go ~ ' } ' 
                ~ ')'
        };

        if $code eq 'infix:<eq>'    { return 'toBool(' 
                                        ~ 'tostr(' ~ (@.arguments[0]).emit_go ~ ') == '
                                        ~ 'tostr(' ~ (@.arguments[1]).emit_go ~ ')' 
                                        ~ ')' 
                                    };
        if $code eq 'infix:<ne>'    { return 'toBool(' 
                                        ~ 'tostr(' ~ (@.arguments[0]).emit_go ~ ') != '
                                        ~ 'tostr(' ~ (@.arguments[1]).emit_go ~ ')' 
                                        ~ ')' 
                                    };
 
        if $code eq 'infix:<==>' { return 'toBool(' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_go ~ ') == '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_go ~ ') '
                                    ~ ')' 
                                };
        if $code eq 'infix:<!=>' { return 'toBool(' 
                                        ~ 'toint(' ~ (@.arguments[0]).emit_go ~ ') != '
                                        ~ 'toint(' ~ (@.arguments[1]).emit_go ~ ') '
                                    ~ ')' 
                                };

        if $code eq 'ternary:<?? !!>' { 
            return 
                'func () *Any { '
                    ~ 'if tobool( ' ~ Call::emit_go_call( @.arguments[0], 'Bool' ) ~ ' ) ' 
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
        $code ~ '( Capture{ p : []*Any{ ' ~ (@.arguments.>>emit_go).join(', ') ~ ' } } )';
    }
}

class Return {
    has $.result;
    method emit_go {
        if ($.result).isa( 'Bind' ) {
            my $tmp := ($.result).parameters;
            return 
                '(func () *Any { '
                    ~ ($.result).emit_go ~ '; ' 
                    ~ 'Go_return(p, ' ~ $tmp.emit_go ~ '); '
                    ~ 'return u_undef(); '
                ~ '}())'
        }
        return
                '(func () *Any { '
                    ~ 'var tmp *Any = ' ~ ($.result).emit_go ~ '; '
                    ~ 'Go_return(p, tmp); '
                    ~ 'return u_undef(); '
                ~ '}())'
    }
    method emit_go_simple {
        if ($.result).isa( 'Bind' ) {
            my $tmp := ($.result).parameters;
            return 
                'return (func () *Any { '
                    ~ ($.result).emit_go ~ '; ' 
                    ~ 'return ' ~ $tmp.emit_go ~ '; '
                ~ '}())'
        }
        return
            'return( ' ~ $.result.emit_go ~ ')';
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
            my $if := If.new( cond => ($cond.arguments)[0], body => @.otherwise, otherwise => @.body );
            return $if.emit_go;
        }
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        my $s := 'if tobool( ' ~ Call::emit_go_call( $cond, 'Bool' ) ~ ' ) { ' 
                    ~ (MiniPerl6::Go::LexicalBlock.new( block => @.body, needs_return => 0 )).emit_go 
                ~ ' }';
        if !(@.otherwise) {
            return $s;
        }
        return $s 
                ~ ' else { ' 
                    ~ (MiniPerl6::Go::LexicalBlock.new( block => @.otherwise, needs_return => 0 )).emit_go 
                ~ ' }';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_go {
          'func (a_ *Any) { ' ~ "\n"
        ~ '  var i = (*(*a_).(array_er).f_array(Capture{})).(*Array); ' ~ "\n"
        ~ '  for pos := 0; pos <= i.n; pos++ { ' ~ "\n"
        ~ '    func (' ~ $.topic.emit_go ~ ' *Any) { ' ~ "\n"
        ~ '      ' ~ (MiniPerl6::Go::LexicalBlock.new( block => @.body, needs_return => 0 )).emit_go ~ "\n"
        ~ '    }(i.v[pos]) ' ~ "\n"
        ~ '  } ' ~ "\n"
        ~ '}(' ~ $.cond.emit_go ~ ')'
    }
}

class When {
    has @.parameters;
    has @.body;
    method emit_go { die "TODO - When" }
}

class While {
    has $.cond;
    has @.body;
    method emit_go { die "TODO - While" }
}

class Leave {
    method emit_go { die "TODO - Leave" }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_go {
        $.var.emit_go;
    }
    method emit_go_init {
        if $.decl eq 'my' {
            my $str := "";
            $str := $str ~ 'var ' ~ ($.var).emit_go ~ ' *Any;' ~ "\n";
            $str := $str ~ ($.var).emit_go ~ ' = ' ~ ($.var).emit_go ~ ';' ~ "\n";
            if ($.var).sigil eq '%' {
                $str := $str ~ ($.var).emit_go ~ ' = h_hash();' ~ "\n";
            }
            else {
            if ($.var).sigil eq '@' {
                $str := $str ~ ($.var).emit_go ~ ' = a_array();' ~ "\n";
            }
            else {
                $str := $str ~ ($.var).emit_go ~ ' = u_undef();' ~ "\n";
            }
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
        'func ' ~ $.name ~ '(v Capture) *Any { ' 
              ~ '    ' ~ ($.sig).emit_go_bind ~ "\n"
              ~ '    p := make(chan *Any); go func () { ' ~ "\n"
              ~ '        ' ~ (MiniPerl6::Go::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_go 
              ~ '; return }(); ' ~ "\n"
              ~ '    return <-p; ' ~ "\n"
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
                'Function( func(v Capture) *Any { '
                    ~ '    ' ~ ($.sig).emit_go_bind ~ "\n"
                    ~ '    p := make(chan *Any); go func () { ' ~ "\n"
                    ~ '        ' ~ (MiniPerl6::Go::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_go 
                    ~ '; return }(); ' ~ "\n"
                    ~ '    return <-p; ' ~ "\n"
                    ~ '} '
                ~ ')'
        }

        'func ' ~ $.name ~ '(v Capture) *Any { ' 
                    ~ '    ' ~ ($.sig).emit_go_bind ~ "\n"
                    ~ '    p := make(chan *Any); go func () { ' ~ "\n"
                    ~ '        ' ~ (MiniPerl6::Go::LexicalBlock.new( block => @.block, needs_return => 1, top_level => 1 )).emit_go 
                    ~ '; return }(); ' ~ "\n"
                    ~ '    return <-p; ' ~ "\n"
        ~ ' }'
    }
}

class Do {
    has @.block;
    method emit_go {
        '(func () *Any { ' 
          ~ (MiniPerl6::Go::LexicalBlock.new( block => @.block, needs_return => 1 )).emit_go 
          ~ '; return u_undef() '
        ~ '})()'
    }
}

class Use {
    has $.mod;
    method emit_go {
        '// use ' ~ $.mod ~ "\n"
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

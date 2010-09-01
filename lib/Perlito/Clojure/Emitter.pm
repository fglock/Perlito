use v6;

class Perlito::Clojure::LexicalBlock {
    has @.block;
    method emit_clojure {
        if !(@.block) {
            return 'nil';
        }
        my $str = '';
        my $has_my_decl = 0;
        my $my_decl = '';
        # my $silence_unused_warning = '';
        for @.block -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $has_my_decl = 1;
                $my_decl = $my_decl ~ '(' ~ ($decl.var).emit_clojure() ~ ' (sv-undef))'; 
                # $silence_unused_warning = $silence_unused_warning ~ ' ' ~ ($decl.var).emit_clojure;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $has_my_decl = 1;
                $my_decl = $my_decl ~ '(' ~ (($decl.parameters).var).emit_clojure() ~ ' (sv-undef))'; 
                # $silence_unused_warning = $silence_unused_warning ~ ' ' ~ (($decl.parameters).var).emit_clojure;
            }
        }
        if $has_my_decl {
            $str = $str ~ '(let (' ~ $my_decl ~ ') ';

            # silence warning "The variable X is defined but never used." in SBCL
            # $str = $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }
        else {
            $str = $str ~ '(do ';
        };
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str = $str ~ ($decl).emit_clojure;
            }
        }; 
        return $str ~ ')';
    }
}


class CompUnit {
    has $.name;
    has %.attributes;
    has %.methods;
    has @.body;
    method emit_clojure {

        my $class_name = Main::to_lisp_namespace($.name);
        my $str = ';; class ' ~ $.name ~ Main.newline;

        $str = $str ~ '(defpackage ' ~ $class_name ~ Main.newline
                ~ '  (:use common-lisp mp-Main))' ~ Main.newline
                ~ ';; (in-package ' ~ $class_name ~ ')' ~ Main.newline;
        # my $silence_unused_warning = '';

        my $has_my_decl = 0;
        my $my_decl = '';
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                $has_my_decl = 1;
                $my_decl = $my_decl ~ '(' ~ ($decl.var).emit_clojure() ~ ' (sv-undef))'; 
                # $silence_unused_warning = $silence_unused_warning ~ ' ' ~ ($decl.var).emit_clojure;
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                $has_my_decl = 1;
                $my_decl = $my_decl ~ '(' ~ (($decl.parameters).var).emit_clojure() ~ ' (sv-undef))'; 
                # $silence_unused_warning = $silence_unused_warning ~ ' ' ~ (($decl.parameters).var).emit_clojure;
            }
        }
        if $has_my_decl {
            $str = $str ~ '(let (' ~ $my_decl ~ ')' ~ Main.newline;

            # silence warning "The variable X is defined but never used." in SBCL
            # $str = $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }

        $str = $str ~ 
'(if (not (ignore-errors (find-class \'' ~ $class_name ~ ')))
  (defclass ' ~ $class_name ~ ' () ()))

(let (x) 
  (setq x (make-instance \'' ~ $class_name ~ '))
  (defun proto-' ~ $class_name ~ ' () x))
';

        my $dumper = '';
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                my $accessor_name = ($decl.var).name;

                $dumper = $dumper ~ '(let ((m (make-instance \'mp-Pair))) '
                    ~ '(setf (sv-key m) "' ~ Main::lisp_escape_string($accessor_name) ~ '") '
                    ~ '(setf (sv-value m) (' ~ Main::to_lisp_identifier($accessor_name) ~ ' self)) m) ';

# suggested by Arthur Lemmens in: http://osdir.com/ml/lisp.lispworks.general/2005-07/msg00153.html 

                $str = $str ~ 
';; has $.' ~ $accessor_name  ~ '
(let ((new-slots (list (list :name \'' ~ Main::to_lisp_identifier($accessor_name)  ~ '
  :readers \'(' ~ Main::to_lisp_identifier($accessor_name)  ~ ')
  :writers \'((setf ' ~ Main::to_lisp_identifier($accessor_name)  ~ '))
  :initform \'(sv-undef)
  :initfunction (constantly (sv-undef))))))
(dolist (slot-defn (sb-mop:class-direct-slots (find-class \'' ~ $class_name  ~ ')))
(push (list :name (sb-mop:slot-definition-name slot-defn)
  :readers (sb-mop:slot-definition-readers slot-defn)
  :writers (sb-mop:slot-definition-writers slot-defn)
  :initform (sb-mop:slot-definition-initform slot-defn)
  :initfunction (sb-mop:slot-definition-initfunction slot-defn))
new-slots))
(sb-mop:ensure-class \'' ~ $class_name  ~ ' :direct-slots new-slots))

';
            }
            if $decl.isa( 'Method' ) {
                my $sig      = $decl.sig;
                my $invocant = $sig.invocant; 
                my $pos      = $sig.positional;
                my $str_specific = '(' ~ $invocant.emit_clojure() ~ ' ' ~ $class_name ~ ')';
                # my $str_generic  =  $invocant.emit_clojure;
                my $str_optionals = '';
                for @$pos -> $field { 
                    $str_optionals = $str_optionals ~ ' ' ~ $field.emit_clojure;
                };
                if ( $str_optionals ) {
                    $str_specific = $str_specific ~ ' &optional' ~ $str_optionals;
                    # $str_generic  = $str_generic  ~ ' &optional' ~ $str_optionals;
                }
                my $block    = Perlito::Clojure::LexicalBlock.new( block => $decl.block );
                $str = $str ~
';; method ' ~ $decl.name() ~ '
(if (not (ignore-errors (find-method \'' ~ Main::to_lisp_identifier($decl.name) ~ ' () ())))
  (defmulti ' ~ Main::to_lisp_identifier($decl.name) ~ ' class)
(defmethod ' ~ Main::to_lisp_identifier($decl.name) ~ ' [' ~ $str_specific ~ ']
  (block mp6-function
    ' ~ $block.emit_clojure() ~ '))

';
            }
            if $decl.isa( 'Sub' ) {
                $str = $str 
                    ~ '(in-package ' ~ $class_name ~ ')' ~ Main.newline
                    ~ '  ' ~ ($decl).emit_clojure() ~ Main.newline
                    ~ '(in-package mp-Main)' ~ Main.newline;
            }
        }; 

        if $.name ne 'Pair' {
            # .perl()
            $str = $str ~ '(defmethod sv-perl ((self ' ~ $class_name ~ '))' ~ Main.newline
                ~ '  (mp-Main::sv-lisp_dump_object "::' ~ Main::lisp_escape_string($.name) ~ '"' 
                ~ ' (list ' ~ $dumper ~ ')))' ~ Main.newline() ~ Main.newline();
        }

        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str = $str ~ ($decl).emit_clojure() ~ Main.newline;
            }
        }; 
        
        if $has_my_decl {
            # close paren for '(let '
            $str = $str ~ ')';
        }
        $str = $str ~ Main.newline() ~ Main.newline();
    }
}

class Val::Int {
    has $.int;
    method emit_clojure { $.int }
}

class Val::Bit {
    has $.bit;
    method emit_clojure { $.bit }
}

class Val::Num {
    has $.num;
    method emit_clojure { $.num }
}

class Val::Buf {
    has $.buf;
    method emit_clojure { '"' ~ Main::lisp_escape_string($.buf) ~ '"' }
}

class Val::Undef {
    method emit_clojure { '(sv-undef)' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_clojure {
        'bless(' ~ %.fields.perl() ~ ', ' ~ $.class.perl() ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit_clojure {
        if @.array1 {
            my $str = '';
            for @.array1 -> $elem {
                if $elem.isa( 'Apply' ) && $elem.code eq 'prefix:<@>' {
                    $str = $str ~ ' ' ~ $elem.emit_clojure;
                }
                else {
                    $str = $str ~ ' (list ' ~ $elem.emit_clojure() ~ ')';
                }
            }
            return '(concatenate \'list ' ~ $str ~ ')';
        }
        else {
            return 'nil'
        }
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_clojure {
        if @.hash1 {
            my $fields = @.hash1;
            my $str = '';
            for @$fields -> $field { 
                $str = $str ~ '(setf (gethash ' ~ ($field[0]).emit_clojure() ~ ' h) ' ~ ($field[1]).emit() ~ ')';
            }; 
            return '(let ((h (make-hash-table :test \'equal))) ' ~ $str ~ ' h)';
        }
        else {
            return '(make-hash-table :test \'equal)';
        }
    }
}

class Lit::Code {
    1;
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_clojure {
        if @.fields {
            my $fields = @.fields;
            my $str = '';
            for @$fields -> $field { 
                $str = $str ~ '(setf (' ~ Main::to_lisp_identifier(($field[0]).buf) ~ ' m) ' ~ ($field[1]).emit_clojure() ~ ')';
            }; 
            '(let ((m (make-instance \'' ~ Main::to_lisp_namespace($.class) ~ '))) ' ~ $str ~ ' m)'
        }
        else {
            return '(make-instance \'' ~ Main::to_lisp_namespace($.class) ~ ')'
        }
    }
}

class Index {
    has $.obj;
    has $.index_exp;
    method emit_clojure {
        return '(elt ' ~ $.obj.emit_clojure() ~ ' ' ~ $.index_exp.emit_clojure() ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_clojure {
        if $.obj.isa( 'Var' ) {
            if ($.obj.name eq 'MATCH') || ($.obj.name eq '/') {
                return '(gethash ' ~ $.index_exp.emit_clojure() ~ ' (sv-hash ' ~ $.obj.emit_clojure() ~ '))';
            }
        };
        return '(gethash ' ~ $.index_exp.emit_clojure() ~ ' ' ~ $.obj.emit_clojure() ~ ')';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_clojure {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $ns = '';
        if $.namespace {
            $ns = Main::to_lisp_namespace( $.namespace ) ~ '::';
        }
           ( $.twigil eq '.' )
        ?? ( '(' ~ Main::to_lisp_identifier( $.name ) ~ ' sv-self)' )
        !!  (    ( $.name eq '/' )
            ??   ( Main::to_lisp_identifier( 'MATCH' ) )
            !!   ( $ns ~ Main::to_lisp_identifier( $.name ) )
            )
    };
}

class Bind {
    has $.parameters;
    has $.arguments;
    method emit_clojure {
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
                $str = $str ~ ' ' ~ $bind.emit_clojure() ~ ' ';
                $i = $i + 1;
            };
            return $str ~ $.parameters.emit_clojure() ~ ' }';
        };
    
        if $.parameters.isa( 'Decl' ) && ( $.parameters.decl eq 'my' ) {
            return '(setf ' ~ ($.parameters.var).emit_clojure() ~ ' ' ~ $.arguments.emit_clojure() ~ ')';
        }
        '(setf ' ~ $.parameters.emit_clojure() ~ ' ' ~ $.arguments.emit_clojure() ~ ')';
    }
}

class Proto {
    has $.name;
    method emit_clojure {
        '(proto-' ~ Main::to_lisp_namespace($.name) ~ ')'
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_clojure {

        my $arguments = '';
        if @.arguments {
            $arguments = (@.arguments.>>emit_clojure).join(' ');
        }

        my $invocant = $.invocant.emit_clojure;
        if $invocant eq 'self' {
            $invocant = 'sv-self';
        };

        if     ($.method eq 'values')
        { 
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return '@{' ~ $invocant ~ '}';
            }
        };

        if $.method eq 'isa' {        
            # (typep "abc" 'Xyz)
            if ((@.arguments[0]).buf) eq 'Str' {
                return '(typep ' ~ $invocant ~ ' \'string)'; 
            }
            return '(typep ' ~ $invocant ~ ' \'' ~ Main::to_lisp_namespace((@.arguments[0]).buf) ~ ')'; 
        }

        if $.method eq 'chars' {
            if ($.hyper) {
                die "not implemented";
            }
            else {
                return '(length ' ~ $invocant ~ ')';
            }
        }

        if     ($.method eq 'yaml')
            || ($.method eq 'say' )
        { 
            if ($.hyper) {
                return 
                    '[ map { ' ~ $.method ~ '( $_, ' ~ ', ' ~ $arguments ~ ')' ~ ' } @{ ' ~ $invocant ~ ' } ]';
            }
            else {
                return
                    '(' ~ $.method ~ ' ' ~ $invocant ~ ' ' ~ $arguments ~ ')';
            }
        };

        my $meth = Main::to_lisp_identifier($.method) ~ ' ';
        if  $.method eq 'postcircumfix:<( )>'  {
             $meth = '';  
        };
        
        if ($.hyper) {
            '(mapcar #\'' ~ $meth ~ $invocant ~ ')';
        }
        else {
            return '(' ~ $meth ~ $invocant ~ ' ' ~ $arguments ~ ')';
        };

    }
}

class Apply {
    has $.code;
    has @.arguments;
    has $.namespace;
    method emit_clojure {
        my $ns = '';
        if $.namespace {
            $ns = Main::to_lisp_namespace( $.namespace ) ~ '::';
        }
        my $code = $ns ~ $.code;
        my $args = '';
        if @.arguments {
            $args = (@.arguments.>>emit_clojure).join(' ');
        }

        if $code eq 'self'       { return 'sv-self' };
        if $code eq 'False'      { return 'nil' };
        if $code eq 'make'       { return '(return-from mp6-function '  ~ $args ~ ')' };
        if $code eq 'substr'     { return '(sv-substr '                 ~ $args ~ ')' };
        if $code eq 'say'        { return '(mp-Main::sv-say (list '     ~ $args ~ '))' };
        if $code eq 'print'      { return '(mp-Main::sv-print (list '   ~ $args ~ '))' };
        if $code eq 'infix:<~>'  { 
            return '(concatenate \'string (sv-string ' ~ (@.arguments[0]).emit_clojure() ~ ') (sv-string ' ~ (@.arguments[1]).emit_clojure() ~ '))'; }
        if $code eq 'warn'       { 
            return '(write-line (format nil "~{~a~}" (list ' ~ $args ~ ')) *error-output*)' };
        if $code eq 'die'        { 
            return '(do (write-line (format nil "~{~a~}" (list ' ~ $args ~ ')) *error-output*) (sb-ext:quit))' };

        if $code eq 'array'      { return $args };

        if $code eq 'prefix:<~>' { return '(sv-string '     ~ $args ~ ')'  };
        if $code eq 'prefix:<!>' { return '(not (sv-bool '  ~ $args ~ '))' };
        if $code eq 'prefix:<?>' { return '(sv-bool '       ~ $args ~ ')'  };

        if $code eq 'prefix:<$>' { return '(sv-scalar '     ~ $args ~ ')'  };
        if $code eq 'prefix:<@>' { return $args };
        if $code eq 'prefix:<%>' { return $args };

        if $code eq 'infix:<+>'  { return '(+ '  ~ $args  ~ ')' };
        if $code eq 'infix:<->'  { return '(-'   ~ $args  ~ ')' };
        if $code eq 'infix:<>>'  { return '(> '  ~ $args  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '(sv-and ' ~ $args ~ ')' };
        if $code eq 'infix:<||>' { return '(sv-or '  ~ $args ~ ')' };
        if $code eq 'infix:<eq>' { return '(sv-eq '  ~ $args ~ ')' };
        if $code eq 'infix:<ne>' { return '(not (sv-eq '  ~ $args ~ '))' };
 
        if $code eq 'infix:<==>' { return '(eql '       ~ $args ~ ')' };
        if $code eq 'infix:<!=>' { return '(not (eql '  ~ $args ~ '))' };

        if $code eq 'ternary:<?? !!>' { 
            return '(if (sv-bool ' ~ (@.arguments[0]).emit_clojure() ~ ') ' ~ (@.arguments[1]).emit_clojure() ~ ' ' ~ (@.arguments[2]).emit_clojure() ~ ')';
        } 
        return '(' ~ $ns ~ Main::to_lisp_identifier($.code) ~ ' ' ~ $args ~ ')';
    }
}

class Return {
    has $.result;
    method emit_clojure {
        return '(return-from mp6-function ' ~ $.result.emit_clojure() ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_clojure {
        my $block1 = Perlito::Clojure::LexicalBlock.new( block => @.body );
        my $block2 = Perlito::Clojure::LexicalBlock.new( block => @.otherwise );
        '(if (sv-bool ' ~ $.cond.emit_clojure() ~ ') ' ~ $block1.emit_clojure() ~ ' ' ~ $block2.emit_clojure() ~ ')';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_clojure {
        my $cond = $.cond;
        my $block = Perlito::Clojure::LexicalBlock.new( block => @.body );
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond = Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        '(dolist (' ~ $.topic.emit_clojure() ~ ' ' ~ $cond.emit_clojure() ~ ') ' ~ $block.emit_clojure() ~ ')';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_clojure {
        my $decl = $.decl;
        my $name = $.var.name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}' )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_clojure;
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_clojure {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_clojure {
        # unused
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_clojure {
        my $sig = $.sig;
        my $pos = $sig.positional;
        my $block = Perlito::Clojure::LexicalBlock.new( block => @.block );
        my $str;
        if @$pos {
            for @$pos -> $field { 
                $str = $str ~ $field.emit_clojure() ~ ' ';
            }
        }
        if $str {
            $str = '&optional ' ~ $str;
        }

        if $.name {
            '(defun ' ~ Main::to_lisp_identifier($.name) ~ ' (' ~ $str ~ ')' ~ Main.newline()
                ~ '  (block mp6-function ' ~ $block.emit_clojure() 
            ~ '))' ~ Main.newline();
        }
        else {
            '(fn ' ~ $.name ~ ' [' ~ $str ~ ']' ~ Main.newline() 
                ~ '  (block mp6-function ' ~ $block.emit_clojure() 
            ~ '))' ~ Main.newline;

        }

    }
}

class Do {
    has @.block;
    method emit_clojure {
        my $block = Perlito::Clojure::LexicalBlock.new( block => @.block );
        return $block.emit_clojure;
    }
}

class Use {
    has $.mod;
    method emit_clojure {
        Main.newline()
        ~ ';; use ' ~ Main::to_lisp_namespace($.mod) ~ Main.newline
    }
}

=begin

=head1 NAME 

Perlito::Clojure::Emit - Code generator for Perlito-in-Clojure 

=head1 SYNOPSIS

    $program.emit_clojure()  # generated Clojure code

=head1 DESCRIPTION

This module generates Clojure code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2009 by Flavio Soibelmann Glock.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

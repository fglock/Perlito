use v6;

class MiniPerl6::Lisp::LexicalBlock {
    has @.block;
    method emit_lisp {
        if !(@.block) {
            return 'nil';
        }
        my $str := '';
        my $has_my_decl := 0;
        my $my_decl := '';
        # my $silence_unused_warning := '';
        my %decl_seen;
        for @.block -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                my $var_name := ($decl.var).emit_lisp;
                if !(%decl_seen{ $var_name }) {
                    $has_my_decl := 1;
                    $my_decl := $my_decl ~ Decl::emit_lisp_initializer( $decl.var );
                    %decl_seen{ $var_name } := 1;
                }
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                my $var_name := (($decl.parameters).var).emit_lisp;
                if !(%decl_seen{ $var_name }) {
                    $has_my_decl := 1;
                    $my_decl := $my_decl ~ Decl::emit_lisp_initializer( ($decl.parameters).var );
                    %decl_seen{ $var_name } := 1;
                }
            }
        }
        if $has_my_decl {
            $str := $str ~ '(let (' ~ $my_decl ~ ') ';

            # silence warning "The variable X is defined but never used." in SBCL
            # $str := $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }
        else {
            $str := $str ~ '(progn ';
        };
        for @.block -> $decl { 
            if (!( $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ))) {
                $str := $str ~ ($decl).emit_lisp;
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
    method emit_lisp {

        my $class_name := Main::to_lisp_namespace($.name);
        my $str := ';; class ' ~ $.name ~ Main.newline;

        my $has_my_decl := 0;
        my $my_decl := '';
        my %decl_seen;
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'my' ) {
                my $var_name := ($decl.var).emit_lisp;
                if !(%decl_seen{ $var_name }) {
                    $has_my_decl := 1;
                    $my_decl := $my_decl ~ Decl::emit_lisp_initializer( $decl.var );
                    %decl_seen{ $var_name } := 1;
                }
            }
            if $decl.isa( 'Bind' ) && ($decl.parameters).isa( 'Decl' ) && ( ($decl.parameters).decl eq 'my' ) {
                my $var_name := (($decl.parameters).var).emit_lisp;
                if !(%decl_seen{ $var_name }) {
                    $has_my_decl := 1;
                    $my_decl := $my_decl ~ Decl::emit_lisp_initializer( ($decl.parameters).var );
                    %decl_seen{ $var_name } := 1;
                }
            }
        }
        if $has_my_decl {
            $str := $str ~ '(let (' ~ $my_decl ~ ')' ~ Main.newline;

            # silence warning "The variable X is defined but never used." in SBCL
            # $str := $str ~ '(list ' ~ $silence_unused_warning ~ ') ';

        }

        $str := $str ~ 
'(if (not (ignore-errors (find-class \'' ~ $class_name ~ ')))
  (defclass ' ~ $class_name ~ ' () ()))

(let (x) 
  (setq x (make-instance \'' ~ $class_name ~ '))
  (defun proto-' ~ $class_name ~ ' () x))
';

        my $dumper := '';
        for @.body -> $decl { 
            if $decl.isa( 'Decl' ) && ( $decl.decl eq 'has' ) {
                my $accessor_name := ($decl.var).name;

                $dumper := $dumper ~ '(let ((m (make-instance \'mp-Pair))) '
                    ~ '(setf (sv-key m) "' ~ Main::lisp_escape_string($accessor_name) ~ '") '
                    ~ '(setf (sv-value m) (' ~ Main::to_lisp_identifier($accessor_name) ~ ' self)) m) ';

# suggested by Arthur Lemmens in: http://osdir.com/ml/lisp.lispworks.general/2005-07/msg00153.html 

                $str := $str ~ 
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
                my $sig      := $decl.sig;
                my $invocant := $sig.invocant; 
                my $pos      := $sig.positional;
                my $str_specific := '(' ~ $invocant.emit_lisp ~ ' ' ~ $class_name ~ ')';
                my $str_generic  :=  $invocant.emit_lisp;
                my $str_optionals := '';
                for @$pos -> $field { 
                    $str_optionals := $str_optionals ~ ' ' ~ $field.emit_lisp;
                };
                if ( $str_optionals ) {
                    $str_specific := $str_specific ~ ' &optional' ~ $str_optionals;
                    $str_generic  := $str_generic  ~ ' &optional' ~ $str_optionals;
                }
                my $block    := MiniPerl6::Lisp::LexicalBlock.new( block => $decl.block );
                $str := $str ~
';; method ' ~ $decl.name ~ '
(if (not (ignore-errors (find-method \'' ~ Main::to_lisp_identifier($decl.name) ~ ' () ())))
  (defgeneric ' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $str_generic ~ ')' ~ Main.newline;
                $str := $str ~ 
'      (:documentation ' ~ '"' ~ 'a method' ~ '"' ~ ')))
(defmethod ' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $str_specific ~ ')
  (block mp6-function
    ' ~ $block.emit_lisp ~ '))

';
            }
            if $decl.isa( 'Sub' ) {
                my $pos := ($decl.sig).positional;
                my $param;
                if @$pos {
                    for @$pos -> $field {
                        $param := $param ~ $field.emit_lisp ~ ' ';
                    }
                }
                my $sig := '';
                if $param {
                    $sig := '&optional ' ~ $param;
                }
                my $block := MiniPerl6::Lisp::LexicalBlock.new( block => $decl.block );
                $str := $str 
                    ~ '(defun ' ~ $class_name ~ '-' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $sig ~ ')' ~ "\n"
                    ~ '  (block mp6-function ' ~ $block.emit_lisp ~ '))' ~ "\n"
                    ~ '(in-package ' ~ $class_name ~ ')' ~ "\n"
                    ~ '  (defun ' ~ Main::to_lisp_identifier($decl.name) ~ ' (' ~ $sig ~ ')' ~ "\n"
                    ~ '    (mp-Main::' ~ $class_name ~ '-' ~ Main::to_lisp_identifier($decl.name) ~ ' ' ~ $param ~ '))' ~ "\n"
                    ~ '(in-package mp-Main)' ~ "\n";
            }
        }; 

        if $.name ne 'Pair' {
            # .perl()
            $str := $str ~ '(defmethod sv-perl ((self ' ~ $class_name ~ '))' ~ Main.newline
                ~ '  (mp-Main::sv-lisp_dump_object "::' ~ Main::lisp_escape_string($.name) ~ '"' 
                ~ ' (list ' ~ $dumper ~ ')))' ~ Main.newline ~ Main.newline;
        }

        for @.body -> $decl { 
            if    (!( $decl.isa( 'Decl' ) && (( $decl.decl eq 'has' ) || ( $decl.decl eq 'my' )) ))
               && (!( $decl.isa( 'Method'))) 
               && (!( $decl.isa( 'Sub'))) 
            {
                $str := $str ~ ($decl).emit_lisp ~ Main.newline;
            }
        }; 
        
        if $has_my_decl {
            # close paren for '(let '
            $str := $str ~ ')';
        }
        $str := $str ~ Main.newline ~ Main.newline;
    }

    sub emit_lisp_program( $comp_units ) {
        my $str := '';
        for @($comp_units) -> $comp_unit {
            $str := $str 
                    ~ '(defpackage ' ~ Main::to_lisp_namespace($comp_unit.name) ~ "\n"
                    ~ '  (:use common-lisp mp-Main))' ~ "\n"
        }
        for @($comp_units) -> $comp_unit {
            $str := $str ~ $comp_unit.emit_lisp ~ "\n"
        }
        return $str;
    }

}

class Val::Int {
    has $.int;
    method emit_lisp { $.int }
}

class Val::Bit {
    has $.bit;
    method emit_lisp { $.bit }
}

class Val::Num {
    has $.num;
    method emit_lisp { $.num }
}

class Val::Buf {
    has $.buf;
    method emit_lisp { '"' ~ Main::lisp_escape_string($.buf) ~ '"' }
}

class Val::Undef {
    method emit_lisp { '(sv-undef)' }
}

class Val::Object {
    has $.class;
    has %.fields;
    method emit_lisp {
        'bless(' ~ %.fields.perl ~ ', ' ~ $.class.perl ~ ')';
    }
}

class Lit::Seq {
    has @.seq;
    method emit_lisp {
        '(' ~ (@.seq.>>emit_lisp).join(' ') ~ ')';
    }
}

class Lit::Array {
    has @.array1;
    method emit_lisp {
        if @.array1 {
            my $str := '';
            for @.array1 -> $elem {
                if     ( $elem.isa( 'Var' )   && $elem.sigil eq '@' )
                    || ( $elem.isa( 'Apply' ) && $elem.code  eq 'prefix:<@>' )
                {
                    $str := $str ~ ' (coerce ' ~ $elem.emit_lisp ~ ' \'list)';
                }
                else {
                    $str := $str ~ ' (list ' ~ $elem.emit_lisp ~ ')';
                }
            }
            return '(let ((_tmp_ (concatenate \'list ' ~ $str ~ '))) ' 
                ~    '(make-array (length _tmp_) :adjustable 1 :fill-pointer t :initial-contents _tmp_))'
        }
        else {
            return '(make-array 0 :adjustable 1 :fill-pointer t)'
        }
    }
}

class Lit::Hash {
    has @.hash1;
    method emit_lisp {
        if @.hash1 {
            my $fields := @.hash1;
            my $str := '';
            for @$fields -> $field { 
                $str := $str ~ '(setf (mp-Main::sv-hash-lookup ' 
                    ~ ($field[0]).emit_lisp ~ ' h) ' ~ ($field[1]).emit_lisp ~ ')';
            }; 
            return '(let ((h (make-hash-table :test \'equal))) ' ~ $str ~ ' h)';
        }
        else {
            return '(make-hash-table :test \'equal)';
        }
    }
}

class Lit::Code {
}

class Lit::Object {
    has $.class;
    has @.fields;
    method emit_lisp {
        if @.fields {
            my $fields := @.fields;
            my $str := '';
            for @$fields -> $field { 
                $str := $str ~ '(setf (' ~ Main::to_lisp_identifier(($field[0]).buf) ~ ' m) ' ~ ($field[1]).emit_lisp ~ ')';
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
    method emit_lisp {
        return '(mp-Main::sv-array-index ' ~ $.obj.emit_lisp ~ ' ' ~ $.index_exp.emit_lisp ~ ')';
    }
}

class Lookup {
    has $.obj;
    has $.index_exp;
    method emit_lisp {
        return '(mp-Main::sv-hash-lookup ' ~ $.index_exp.emit_lisp ~ ' ' ~ $.obj.emit_lisp ~ ')';
    }
}

class Var {
    has $.sigil;
    has $.twigil;
    has $.namespace;
    has $.name;
    method emit_lisp {
        # Normalize the sigil here into $
        # $x    => $x
        # @x    => $List_x
        # %x    => $Hash_x
        # &x    => $Code_x
        my $ns := '';
        if $.namespace {
            $ns := Main::to_lisp_namespace( $.namespace ) ~ '::';
        }
        else {
            if ($.sigil eq '@') && ($.twigil eq '*') && ($.name eq 'ARGS') {
                return '*mp6-args*'
            }
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
    method emit_lisp {
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
                    parameters => $var[1], 
                    arguments  => Call.new( invocant => $b, method => ($var[0]).buf, arguments => [ ], hyper => 0 )
                );
                $str := $str ~ ' ' ~ $bind.emit_lisp ~ ' ';
                $i := $i + 1;
            };
            return $str ~ $.parameters.emit_lisp ~ ' }';
        };
    
        if $.parameters.isa( 'Decl' ) && ( $.parameters.decl eq 'my' ) {
            return '(setf ' ~ ($.parameters.var).emit_lisp ~ ' ' ~ $.arguments.emit_lisp ~ ')';
        }
        '(setf ' ~ $.parameters.emit_lisp ~ ' ' ~ $.arguments.emit_lisp ~ ')';
    }
}

class Proto {
    has $.name;
    method emit_lisp {
        '(proto-' ~ Main::to_lisp_namespace($.name) ~ ')'
    }
}

class Call {
    has $.invocant;
    has $.hyper;
    has $.method;
    has @.arguments;
    method emit_lisp {

        my $arguments := (@.arguments.>>emit_lisp).join(' ');

        my $invocant := $.invocant.emit_lisp;
        if $invocant eq 'self' {
            $invocant := 'sv-self';
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

        my $meth := Main::to_lisp_identifier($.method) ~ ' ';
        if  $.method eq 'postcircumfix:<( )>'  {
             return '(funcall ' ~ $invocant ~ ' ' ~ $arguments ~ ')';
        };
        
        if ($.hyper) {
            return '(map \'vector #\'(lambda (c) (' ~ $meth ~ ' c)) ' ~ $invocant ~ ')'
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
    method emit_lisp {
        my $ns := '';
        if $.namespace {
            $ns := Main::to_lisp_namespace( $.namespace ) ~ '::';
        }
        my $code := $ns ~ $.code;

        if $code eq 'infix:<~>'  { 
            return '(concatenate \'string (sv-string ' ~ (@.arguments[0]).emit_lisp ~ ') (sv-string ' ~ (@.arguments[1]).emit_lisp ~ '))'; }
        if $code eq 'ternary:<?? !!>' { 
            return '(if (sv-bool ' ~ (@.arguments[0]).emit_lisp ~ ') ' ~ (@.arguments[1]).emit_lisp ~ ' ' ~ (@.arguments[2]).emit_lisp ~ ')';
        } 

        my $args := '';
        if @.arguments {
            $args := (@.arguments.>>emit_lisp).join(' ');
        }

        if $code eq 'self'       { return 'sv-self' };
        if $code eq 'false'      { return 'nil' };
        if $code eq 'make'       { return '(setf (sv-capture sv-MATCH) '  ~ $args ~ ')' };
        if $code eq 'substr'     { return '(sv-substr '                 ~ $args ~ ')' };
        if $code eq 'say'        { return '(mp-Main::sv-say (list '     ~ $args ~ '))' };
        if $code eq 'print'      { return '(mp-Main::sv-print (list '   ~ $args ~ '))' };
        if $code eq 'warn'       { 
            return '(write-line (format nil "~{~a~}" (list ' ~ $args ~ ')) *error-output*)' };
        if $code eq 'die'        { 
            return '(progn (write-line (format nil "~{~a~}" (list ' ~ $args ~ ')) *error-output*) (sb-ext:quit))' };

        if $code eq 'array'      { return $args };

        if $code eq 'exists'     {
                                      my $arg := @.arguments[0];
                                      if $arg.isa( 'Lookup' ) {
                                        return '(nth-value 1 ' ~ $arg.emit_lisp ~ ')'
                                      }
                                 }

        if $code eq 'prefix:<~>' { return '(sv-string '     ~ $args ~ ')'  };
        if $code eq 'prefix:<!>' { return '(not (sv-bool '  ~ $args ~ '))' };
        if $code eq 'prefix:<?>' { return '(sv-bool '       ~ $args ~ ')'  };

        if $code eq 'prefix:<$>' { return '(sv-scalar '     ~ $args ~ ')'  };
        if $code eq 'prefix:<@>' { return $args };
        if $code eq 'prefix:<%>' { return $args };

        if $code eq 'infix:<+>'  { return '(sv-add ' ~ $args  ~ ')' };
        if $code eq 'infix:<->'  { return '(- '   ~ $args  ~ ')' };
        if $code eq 'infix:<>>'  { return '(> '   ~ $args  ~ ')' };
        if $code eq 'infix:<<>'  { return '(< '   ~ $args  ~ ')' };
        if $code eq 'infix:<>=>' { return '(>= '  ~ $args  ~ ')' };
        if $code eq 'infix:<<=>' { return '(<= '  ~ $args  ~ ')' };
        
        if $code eq 'infix:<&&>' { return '(sv-and ' ~ $args ~ ')' };
        if $code eq 'infix:<||>' { return '(sv-or '  ~ $args ~ ')' };
        if $code eq 'infix:<eq>' { return '(sv-eq '  ~ $args ~ ')' };
        if $code eq 'infix:<ne>' { return '(not (sv-eq '  ~ $args ~ '))' };
 
        if $code eq 'infix:<==>' { return '(sv-eq-int '       ~ $args ~ ')' };
        if $code eq 'infix:<!=>' { return '(not (sv-eq-int '  ~ $args ~ '))' };

        return '(' ~ $ns ~ Main::to_lisp_identifier($.code) ~ ' ' ~ $args ~ ')';
    }
}

class Return {
    has $.result;
    method emit_lisp {
        return '(return-from mp6-function ' ~ $.result.emit_lisp ~ ')';
    }
}

class If {
    has $.cond;
    has @.body;
    has @.otherwise;
    method emit_lisp {
        my $block1 := MiniPerl6::Lisp::LexicalBlock.new( block => @.body );
        my $block2 := MiniPerl6::Lisp::LexicalBlock.new( block => @.otherwise );
        '(if (sv-bool ' ~ $.cond.emit_lisp ~ ') ' ~ $block1.emit_lisp ~ ' ' ~ $block2.emit_lisp ~ ')';
    }
}

class For {
    has $.cond;
    has @.body;
    has @.topic;
    method emit_lisp {
        my $cond := $.cond;
        my $block := MiniPerl6::Lisp::LexicalBlock.new( block => @.body );
        if   $cond.isa( 'Var' ) 
          && $cond.sigil eq '@' 
        {
            $cond := Apply.new( code => 'prefix:<@>', arguments => [ $cond ] );
        };
        # '(dolist (' ~ $.topic.emit_lisp ~ ' ' ~ $cond.emit_lisp ~ ') ' ~ $block.emit_lisp ~ ')';
        '(loop for ' 
            ~ $.topic.emit_lisp 
            ~ ' across ' ~ $cond.emit_lisp ~ 
            ' do ' ~ $block.emit_lisp ~ ')';

        # (loop for x being the elements of '(1 2 3) 
        #        do (print x))

    }
}

class While {
    has $.cond;
    has @.body;
    method emit_lisp { 
          '(loop while (sv-bool ' ~ $.cond.emit_lisp ~ ') do ' 
        ~    (MiniPerl6::Lisp::LexicalBlock.new( block => @.body )).emit_lisp
        ~ ')';
    }
}

class Decl {
    has $.decl;
    has $.type;
    has $.var;
    method emit_lisp {
        my $decl := $.decl;
        my $name := $.var.name;
           ( $decl eq 'has' )
        ?? ( 'sub ' ~ $name ~ ' { ' ~
            '@_ == 1 ' ~
                '? ( $_[0]->{' ~ $name ~ '} ) ' ~
                ': ( $_[0]->{' ~ $name ~ '} = $_[1] ) ' ~
            '}' )
        !! $.decl ~ ' ' ~ $.type ~ ' ' ~ $.var.emit_lisp;
    }
    sub emit_lisp_initializer ($decl) {
        if $decl.sigil eq '%' {
            return '(' ~ $decl.emit_lisp ~ ' (make-hash-table :test \'equal))'; 
        }
        else {
        if $decl.sigil eq '@' {
            return '(' ~ $decl.emit_lisp ~ ' (make-array 0 :fill-pointer t :adjustable t))'; 
        }
        else {
            return '(' ~ $decl.emit_lisp ~ ' (sv-undef))'; 
        }
        }
    }
}

class Sig {
    has $.invocant;
    has $.positional;
    has $.named;
    method emit_lisp {
        ' print \'Signature - TODO\'; die \'Signature - TODO\'; '
    };
}

class Method {
    has $.name;
    has $.sig;
    has @.block;
    method emit_lisp {
        # unused
    }
}

class Sub {
    has $.name;
    has $.sig;
    has @.block;
    method emit_lisp {
        my $sig := $.sig;
        my $pos := $sig.positional;
        my $block := MiniPerl6::Lisp::LexicalBlock.new( block => @.block );
        my $str;
        if @$pos {
            for @$pos -> $field { 
                $str := $str ~ $field.emit_lisp ~ ' ';
            }
        }
        if $str {
            $str := '&optional ' ~ $str;
        }

        if $.name {
            '(defun ' ~ Main::to_lisp_identifier($.name) ~ ' (' ~ $str ~ ')' ~ Main.newline 
                ~ '  (block mp6-function ' ~ $block.emit_lisp 
            ~ '))' ~ Main.newline;
        }
        else {
            '(lambda ' ~ $.name ~ ' (' ~ $str ~ ')' ~ Main.newline 
                ~ '  (block mp6-function ' ~ $block.emit_lisp 
            ~ '))' ~ Main.newline;

        }

    }
}

class Do {
    has @.block;
    method emit_lisp {
        my $block := MiniPerl6::Lisp::LexicalBlock.new( block => @.block );
        return $block.emit_lisp;
    }
}

class Use {
    has $.mod;
    method emit_lisp {
        Main.newline
        ~ ';; use ' ~ Main::to_lisp_namespace($.mod) ~ Main.newline
    }
}

=begin

=head1 NAME 

MiniPerl6::Lisp::Emit - Code generator for MiniPerl6-in-Lisp (SBCL)

=head1 SYNOPSIS

    $program.emit_lisp  # generated Lisp code

=head1 DESCRIPTION

This module generates Lisp code for the MiniPerl6 compiler.

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

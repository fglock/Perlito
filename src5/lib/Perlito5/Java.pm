
package Perlito5::Java;

use v5;
use Perlito5::AST;
use Perlito5::AST::Captures;
use Perlito5::Dumper;
use Perlito5::Java::Emitter;
use Perlito5::Java::Apply;
use Perlito5::Perl5::Emitter;       # TODO - cleanup the dependency on Perl5 emitter
use Perlito5::Perl5::PrettyPrinter; # TODO - cleanup the dependency on Perl5 emitter
use strict;

sub init {
    Perlito5::Java::init_java_class();
}

my %label;

# 'The::Class' => {
#       import              => 'full.path.Class',   # Java class path
#       perl_package        => 'The::Class',        # Perl package name
#       java_type           => 'Class',             # generated, can be overridden: 'Class<String>'
#       perl_to_java        => 'to_TheClass',       # generated, can be overridden
#       java_native_to_perl => 'pClass',            # generated
# }
my %Java_class;

our %Java_var_name; # 101 => 'this.env.[0]'
my %Java_var;       # 101 => { id => 101, type => 'Byte' }
our @Java_init;
our $is_inside_subroutine;  # 'shift @_' vs. 'shift @ARGV'

sub pkg {
    Perlito5::Java::escape_string($Perlito5::PKG_NAME )
}
sub get_label {
    'tmp' . $Perlito5::ID++
}
sub tab {
    my $level = shift;
    "    " x $level
}
sub get_java_class_info {
    return \%Java_class;
}
sub get_java_var_info {
    return \%Java_var;
}
sub set_java_class_defaults {
    my ($perl_package, $java_import) = @_;
    # import              => 'full.path.Class',   # Java class path
    # perl_package        => 'The::Class',        # Perl package name
    # java_type           => 'Class',             # generated, can be overridden: 'Class<String>'
    # perl_to_java        => 'to_TheClass',       # generated, can be overridden
    # java_native_to_perl => 'pClass',            # generated
    #
    my $Java_class = Perlito5::Java::get_java_class_info();
    my @parts = split /\./, $java_import;
    $Java_class->{$perl_package}->{java_type} //= $parts[-1];
    $Java_class->{$perl_package}->{java_native_to_perl} //= 'p' . $Java_class->{$perl_package}->{java_type};
    # "List<String>" becomes "PlList_String_"
    $Java_class->{$perl_package}->{java_native_to_perl} =~ s/[<>]/_/g;
    my $perl_to_java = $perl_package;
    $perl_to_java =~ s/:://g;
    $Java_class->{$perl_package}->{perl_to_java} //= "to_${perl_to_java}";
    $Java_class->{$perl_package}->{perl_package} = $perl_package;
}
sub init_java_class {
    my $Java_class = Perlito5::Java::get_java_class_info();
    $Java_class->{String} = {
        java_type           => 'String',
        java_native_to_perl => 'PlString',
        perl_to_java        => 'toString',
        perl_package        => 'String',
    };
    $Java_class->{Long} = {
        java_type           => 'Long',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_long',
        perl_package        => 'Long',
    };
    $Java_class->{Integer} = {
        java_type           => 'Integer',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_int',
        perl_package        => 'Integer',
    };
    $Java_class->{Boolean} = {
        java_type           => 'Boolean',
        java_native_to_perl => 'PlBool',
        perl_to_java        => 'to_boolean',
        perl_package        => 'Boolean',
    };
    $Java_class->{Double} = {
        java_type           => 'Double',
        java_native_to_perl => 'PlDouble',
        perl_to_java        => 'to_double',
        perl_package        => 'Double',
    };
    $Java_class->{Byte} = {
        java_type           => 'Byte',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_byte',
        perl_package        => 'Byte',
    };
    $Java_class->{Short} = {
        java_type           => 'Short',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_short',
        perl_package        => 'Short',
    };
    $Java_class->{Float} = {
        java_type           => 'Float',
        java_native_to_perl => 'PlDouble',
        perl_to_java        => 'to_float',
        perl_package        => 'Float',
    };
    # # $Java_class->{Object} = {
    # #     java_type           => 'Object',
    # #     java_native_to_perl => 'PlObject',
    # #     perl_to_java        => 'to_object',
    # #     perl_package        => 'Object',
    # # };
    #
    #  - "java.lang.Object" can be imported using the standard import syntax:
    #
    #    package Java::Object { import => "java.lang.Object" }; 
    #    my Java::Object $obj = Java::Object->new();
    #
    $Java_class->{Character} = {
        java_type           => 'Character',
        java_native_to_perl => 'PlString',
        perl_to_java        => 'to_char',
        perl_package        => 'Character',
    };
    $Java_class->{long} = {
        java_type           => 'long',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_long',
        perl_package        => 'long',
    };
    $Java_class->{int} = {
        java_type           => 'int',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_int',
        perl_package        => 'int',
    };
    $Java_class->{short} = {
        java_type           => 'short',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_long',
        perl_package        => 'short',
    };
    $Java_class->{byte} = {
        java_type           => 'byte',
        java_native_to_perl => 'PlInt',
        perl_to_java        => 'to_long',
        perl_package        => 'byte',
    };
    $Java_class->{boolean} = {
        java_type           => 'boolean',
        java_native_to_perl => 'PlBool',
        perl_to_java        => 'to_boolean',
        perl_package        => 'boolean',
    };
    $Java_class->{double} = {
        java_type           => 'double',
        java_native_to_perl => 'PlDouble',
        perl_to_java        => 'to_double',
        perl_package        => 'double',
    };
    $Java_class->{float} = {
        java_type           => 'float',
        java_native_to_perl => 'PlDouble',
        perl_to_java        => 'to_float',
        perl_package        => 'float',
    };
    $Java_class->{char} = {
        java_type           => 'char',
        java_native_to_perl => 'PlString',
        perl_to_java        => 'to_char',
        perl_package        => 'char',
    };
}

our %Java_loop_label;
sub get_java_loop_label {
    my $s = shift;
    return 0 if !$s;
    return $Java_loop_label{$s} if exists $Java_loop_label{$s};
    my $label = $Perlito5::ID++;
    $Java_loop_label{$s} = $label;
    # push @Perlito5::Java::Java_constants, "public static final int Loop$s = $label;";
    return $label;
}

our %Java_constant_seen;
sub get_constant {
    my ($type, $create) = @_;
    if (exists $Java_constant_seen{$create}) {
        return $Java_constant_seen{$create};
    }
    my $label = Perlito5::Java::get_label();
    push @Perlito5::Java::Java_constants, "public static final $type $label = $create;";
    $Java_constant_seen{$create} = $label;
    return $label;
}

# prefix operators that take a "str" parameter
our %op_prefix_js_str = (
    'prefix:<-A>' => 'PerlOp.p5atime',
    'prefix:<-C>' => 'PerlOp.p5ctime',
    'prefix:<-M>' => 'PerlOp.p5mtime',
    'prefix:<-d>' => 'PerlOp.p5is_directory',
    'prefix:<-e>' => 'PerlOp.p5file_exists',
    'prefix:<-f>' => 'PerlOp.p5is_file',
    'prefix:<-s>' => 'PerlOp.p5size',
    'prefix:<-l>' => 'PerlOp.p5is_symbolic_link',
    'prefix:<-r>' => 'PerlOp.p5is_readable',
    'prefix:<-w>' => 'PerlOp.p5is_writable',
    'prefix:<-x>' => 'PerlOp.p5is_executable',
    'prefix:<-p>' => 'PerlOp.p5is_named_pipe',
);

# these operators need 2 "str" parameters
our %op_infix_js_str = (
    'infix:<eq>' => ' == ',
    'infix:<ne>' => ' != ',
    'infix:<le>' => ' <= ',
    'infix:<ge>' => ' >= ',
    'infix:<lt>' => ' < ',
    'infix:<gt>' => ' > ',
);
# these operators always return "bool"
our %op_to_boolean = map +($_ => 1), qw(
    prefix:<!>
    infix:<!=>
    infix:<==>
    infix:<<=>
    infix:<>=>
    infix:<>>
    infix:<<>
    infix:<eq>
    infix:<ne>
    infix:<ge>
    infix:<le>
    infix:<gt>
    infix:<lt>
    prefix:<not>
    exists
    defined
);
# these operators always return "string"
our %op_to_str = map +($_ => 1), qw(
    substr
    join
    list:<.>
    chr
    lc
    uc
    lcfirst
    ucfirst
    ref
);
# these operators always return "num"
our %op_to_num = map +($_ => 1), qw(
    length
    index
    rindex
    ord
    oct
    int
    infix:<->
    infix:<+>
    infix:<*>
    infix:</>
    infix:<%>
    infix:<**>
    infix:<|>
    infix:<&>
);
# these operators always return "scalar"
our %op_to_scalar = map +($_ => 1), (
    keys %op_to_str,
    keys %op_to_num,
    keys %op_to_boolean,
    'circumfix:<[ ]>',
    'circumfix:<{ }>',
    qw(
        infix:<cmp>
        infix:<<=>>
        postfix:<++>
        postfix:<-->
        prefix:<++>
        prefix:<-->
        bless
    ),
    # exceptions:
    #   'prefix:<\\>' because '\(@a)' is a list
    #   'infix:<=>'   depends on the sigil
);
# these operators will generate native Java code when possible
our %native_op = qw(
    infix:<->   -
    infix:<+>   +
    infix:<*>   *
    infix:</>   /
    infix:<!=>  !=
    infix:<==>  ==
    infix:<<=>  <=
    infix:<>=>  >=
    infix:<>>   >
    infix:<<>   <
);
our %native_op_unary = qw(
    postfix:<++>    1  
    postfix:<-->    1
    prefix:<++>     1
    prefix:<-->     1 
); 
# these operators will generate native Java code when possible; return "boolean"
our %native_op_to_boolean = qw(
    infix:<!=>  !=
    infix:<==>  ==
    infix:<<=>  <=
    infix:<>=>  >=
    infix:<>>   >
    infix:<<>   <
);
our %valid_java_statement = qw(
    delete          1
    die             1
    do              1
    infix:<=>       1
    last            1
    next            1
    postfix:<++>    1
    postfix:<-->    1
    prefix:<++>     1
    prefix:<-->     1
    print           1
    printf          1
    push            1
    redo            1
    return          1
    say             1
    shift           1
    tie             1
    unshift         1
    untie           1
    warn            1
); 
# these variables will be optimized
our %special_scalar = (
    '_'  => "Scalar_ARG",  # $_
    '\\' => "Scalar_OUTPUT_RECORD_SEPARATOR",  # $\
);

my %safe_char = (
    ' ' => 1,
    '!' => 1,
    '#' => 1,
    '$' => 1,
    '%' => 1,
    '&' => 1,
    '(' => 1,
    ')' => 1,
    '*' => 1,
    '+' => 1,
    ',' => 1,
    '-' => 1,
    '.' => 1,
    '/' => 1,
    ':' => 1,
    ';' => 1,
    '<' => 1,
    '=' => 1,
    '>' => 1,
    '?' => 1,
    '@' => 1,
    '[' => 1,
    ']' => 1,
    '^' => 1,
    '_' => 1,
    '`' => 1,
    '{' => 1,
    '|' => 1,
    '}' => 1,
    '~' => 1,
);

sub escape_string {
    my $s = shift;
    my @out;
    my $tmp = '';
    my $has_char = 0;
    return '""' if $s eq '';
    for my $i (0 .. length($s) - 1) {
        my $c = substr($s, $i, 1);
        if  (  ($c ge 'a' && $c le 'z')
            || ($c ge 'A' && $c le 'Z')
            || ($c ge '0' && $c le '9')
            || exists( $safe_char{$c} )
            )
        {
            $tmp = $tmp . $c;
        }
        elsif (ord($c) > 65535) {

            # this is necessary to support characters with code > 65535
            # new String(Character.toChars((int)(1114109L)))

            push @out, "\"$tmp\"" if $tmp ne '';
            push @out, "new String(Character.toChars(" . ord($c) . "))";
            $has_char = 1;
            $tmp = '';
        }
        else {
            push @out, "\"$tmp\"" if $tmp ne '';
            push @out, "(char)" . ord($c) . "";
            $has_char = 1;
            $tmp = '';
        }
    }
    push @out, "\"$tmp\"" if $tmp ne '';
    unshift @out, '""' if $has_char;
    return $out[0] if @out == 1;
    return '(' . join(' + ', @out) . ')';
}

sub is_native {
    my $self = shift;

    if ( ref($self) eq 'Perlito5::AST::Call' ) {

        # class method call in native 'Java' packages
        #
        #   package Sample { import => "misc.Java.Sample" };
        #   Sample->new();  
        #   new Sample();
        #
        if ( ref($self->{invocant}) eq 'Perlito5::AST::Var' && $self->{invocant}->{sigil} eq '::' ) {
            my $Java_class = Perlito5::Java::get_java_class_info();
            if ( exists $Java_class->{$self->{invocant}->{namespace}} ) {
                return 1;
            }
        }

        # method call on a typed invocant
        #   package Sample { import => "misc.Java.Sample" };
        #   my Sample $s;  
        #   $s->meth();
        #
        if ( ref($self->{invocant}) eq 'Perlito5::AST::Var' && $self->{invocant}->{_id} ) {
            my $id = $self->{invocant}->{_id};
            my $Java_var = Perlito5::Java::get_java_var_info();
            my $type = $Java_var->{ $id }{type} || 'PlLvalue';
            if ($type ne 'PlLvalue') {
                return 1;
            }
        }

        # method call on a "is_native" invocant
        #   package Sample { import => "misc.Java.Sample" };
        #   my Sample $s;  
        #   $s->meth()->meth();
        #
        if ( is_native($self->{invocant}) ) {
            return 1;
        }

        # type coercion method call on an untyped invocant
        #   package Sample { import => "misc.Java.Sample" };
        #   my $x;  
        #   $x->to_Sample();
        #
        my $meth = $self->{method}; 
        if ( $meth =~ /^to/ ) {
            # TODO - check for no-arguments
            my $Java_class = Perlito5::Java::get_java_class_info();
            for my $info ( values %{$Java_class} ) {
                if ( $meth eq $info->{perl_to_java} ) {
                    return 1;
                }
            }
        }

    }

    return 0; # <- not native (plain Perl)
}

sub is_native_bool {
    my $self = shift;

    # if (is_native($self)) {
    #     return 1;
    # }
    if (is_native_args([$self])) {
        return 1;
    }
    my $is_apply = $self->isa( 'Perlito5::AST::Apply' ) && $self->{arguments} && @{$self->{arguments}};
    if ($is_apply && exists $native_op_to_boolean{ $self->{code} } && is_native_args($self->{arguments})) {
        return 1;
    }
    return 0;
}

sub to_native_args {
        my $args = shift;
        my $level = shift;
        my $wantarray = 'scalar';
        my $s = '';
        my @out;

        for my $cond (@$args) {
            my $is_apply = $cond->isa( 'Perlito5::AST::Apply' ) && $cond->{arguments} && @{$cond->{arguments}};

            if ( $is_apply && $cond->code eq 'circumfix:<( )>') {
                push @out, to_native_args( $cond->{arguments}[0], $level );
            }
            elsif ( $is_apply && exists $native_op{ $cond->code } ) {
                # TODO - cast arguments to "number", "string" or "boolean" depending on operator
                push @out, '(' . to_native_num($cond->{arguments}[0], $level, $wantarray) .
                    ' ' . $native_op{ $cond->code } . ' ' . to_native_num($cond->{arguments}[1], $level, $wantarray) . ')';
            }
            elsif ( $is_apply && exists $op_to_num{ $cond->code } ) {
                push @out, '(' . $cond->emit_java($level, $wantarray) . ').' .
                    (${$cond->{arguments}}[0]->isa( 'Perlito5::AST::Num' ) || ${$cond->{arguments}}[1]->isa( 'Perlito5::AST::Num' )
                        ? 'to_double()' : 'to_long()');
            }
            elsif ( $is_apply && exists $op_to_str{ $cond->code } ) {
                push @out, '(' . $cond->emit_java($level, $wantarray) . ').toString()';
            }
            elsif ( $cond->isa( 'Perlito5::AST::Apply' ) && $cond->{code} eq 'undef' ) {
                push @out, 'null';
            }
            elsif ($cond->isa( 'Perlito5::AST::Buf' )) {
                push @out, Perlito5::Java::escape_string( $cond->{buf} );
            }
            elsif ($cond->isa( 'Perlito5::AST::Int' )) {
                push @out, $cond->{int};
            }
            elsif ($cond->isa( 'Perlito5::AST::Num' )) {
                push @out, $cond->{num};
            }
            else {
                push @out, $cond->emit_java($level, $wantarray);
            }
        }
        return join(', ', @out);
}

sub is_native_args {
        my $args = shift;
        my $wantarray = 'scalar';
        my $s = '';
        my @out;

        for my $cond (@$args) {
            my $is_apply = $cond->isa( 'Perlito5::AST::Apply' ) && $cond->{arguments} && @{$cond->{arguments}};

            if ( $is_apply && $cond->code eq 'circumfix:<( )>') {
                return 0 unless is_native_args($cond->{arguments});
            }
            elsif ( $is_apply && exists $native_op{ $cond->code } ) {
                return 0 unless is_native_args($cond->{arguments});
            }
            elsif ( $is_apply && exists $native_op_unary{ $cond->code } ) {
                return 0 unless is_native_args($cond->{arguments});
            }
            # elsif ( $is_apply && exists $op_to_num{ $cond->code } ) {
            #     push @out, '(' . $cond->emit_java($level, $wantarray) . ').' .
            #         (${$cond->{arguments}}[0]->isa( 'Perlito5::AST::Num' ) || ${$cond->{arguments}}[1]->isa( 'Perlito5::AST::Num' )
            #             ? 'to_double()' : 'to_long()');
            # }
            # elsif ( $is_apply && exists $op_to_str{ $cond->code } ) {
            #     push @out, '(' . $cond->emit_java($level, $wantarray) . ').toString()';
            # }
            # elsif ( $cond->isa( 'Perlito5::AST::Apply' ) && $cond->{code} eq 'undef' ) {
            #     push @out, 'null';
            # }
            # elsif ($cond->isa( 'Perlito5::AST::Buf' )) {
            #     push @out, Perlito5::Java::escape_string( $cond->{buf} );
            # }
            elsif ($cond->isa( 'Perlito5::AST::Int' )) {
                ;
            }
            elsif ($cond->isa( 'Perlito5::AST::Num' )) {
                ;
            }
            elsif ( ref($cond) eq 'Perlito5::AST::Var' && $cond->{_id} ) {
                my $id = $cond->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type eq 'PlLvalue') {
                    return 0;
                }
            }
            else {
                return 0 unless is_native($cond);
            }
        }
        return 1 if @$args;
        return 0;
}

sub to_native_bool {
        my $cond = shift;
        my $level = shift;
        my $wantarray = shift;
        if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
           && $cond->{arguments} && @{$cond->{arguments}}
           )
        {
            return to_native_bool( $cond->{arguments}[0], $level, $wantarray )
        }
        elsif ($cond->isa( 'Perlito5::AST::Int' )) {
            if ($cond->{int} == 0) {
                return 'false';
            }
            return '(' . $cond->{int} . ' != 0)';
        }
        elsif ($cond->isa( 'Perlito5::AST::Num' )) {
            if ($cond->{num} == 0.0) {
                return 'false';
            }
            return '(' . $cond->{num} . ' != 0.0)';
        }
        else {
            # TODO - ensure "bool"
            return to_native_args([$cond], $level);
        }
}

sub to_native_num {
        my $cond = shift;
        my $level = shift;
        my $wantarray = shift;
        if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
           && $cond->{arguments} && @{$cond->{arguments}}
           )
        {
            return to_native_num( $cond->{arguments}[0], $level, $wantarray )
        }
        elsif ($cond->isa( 'Perlito5::AST::Int' )) {
            return $cond->{int};
        }
        elsif ($cond->isa( 'Perlito5::AST::Num' )) {
            return $cond->{num};
        }
        else {
            # TODO - ensure "num"
            return to_native_args([$cond], $level);
        }
}

sub to_native_str {
        my $cond = shift;
        my $level = shift;
        my $wantarray = shift;
        if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
           && $cond->{arguments} && @{$cond->{arguments}}
           )
        {
            return to_native_str( $cond->{arguments}[0], $level, $wantarray )
        }
        if ($cond->isa( 'Perlito5::AST::Buf' )) {
            return Perlito5::Java::escape_string( $cond->{buf} );
        }
        elsif ($cond->isa( 'Perlito5::AST::Int' )) {
            return Perlito5::Java::escape_string( $cond->{int} );
        }
        elsif ($cond->isa( 'Perlito5::AST::Num' )) {
            return Perlito5::Java::escape_string( $cond->{num} );
        }
        else {
            return $cond->emit_java($level, $wantarray) . '.toString()';
        }
}

sub to_array_index {
        my $cond = shift;
        my $level = shift;
        my $wantarray = shift;
        if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
           && $cond->{arguments} && @{$cond->{arguments}}
           )
        {
            return to_array_index( $cond->{arguments}[0], $level, $wantarray )
        }
        if ($cond->isa( 'Perlito5::AST::Buf' )) {
            return int( 0 + $cond->{buf} );
        }
        elsif ($cond->isa( 'Perlito5::AST::Int' )) {
            return int( 0 + $cond->{int} );
        }
        elsif ($cond->isa( 'Perlito5::AST::Num' )) {
            return int( 0 + $cond->{num} );
        }
        else {
            return $cond->emit_java($level, $wantarray);
        }
}

sub to_str {
        my $cond = shift;
        my $level = shift;
        my $wantarray = 'scalar';
        if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
           && $cond->{arguments} && @{$cond->{arguments}}
           ) 
        {
            return to_str( $cond->{arguments}[0], $level )
        }

        if  (  ($cond->isa( 'Perlito5::AST::Buf' ))
            || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_str{ $cond->code } )
            )
        {
            return $cond->emit_java($level, $wantarray);
        }
        else {
            return 'new PlString(' . $cond->emit_java($level, $wantarray) . '.toString())';
        }
}
sub to_num {
        my $cond = shift;
        my $level = shift;
        my $type = shift;
        my $wantarray = 'scalar';
        if  (  $cond->isa( 'Perlito5::AST::Int' ) 
            || $cond->isa( 'Perlito5::AST::Num' )
            || ($cond->isa( 'Perlito5::AST::Apply' )  && exists $op_to_num{ $cond->code } )
            )
        {
            return $cond->emit_java($level, $wantarray);
        }
        else {
            # TODO - this converts to "double" - it should be int/double depending on context
            if ($type eq 'int') {
                return 'new PlInt(' . $cond->emit_java($level, $wantarray) . '.to_long())';
            }
            return 'new PlDouble(' . $cond->emit_java($level, $wantarray) . '.to_double())';
        }
}
sub to_boolean {
        my $cond = shift;
        my $level = shift;
        my $wantarray = 'scalar';
        my $class = ref($cond);

        if (  $class eq 'Perlito5::AST::Apply' ) {

            if (  $cond->code eq 'circumfix:<( )>'
               && $cond->{arguments} && @{$cond->{arguments}}
               ) 
            {
                if (@{$cond->{arguments}} == 1) {
                    return to_boolean( $cond->{arguments}[0], $level );
                }
            }

            # Note: 'infix:<||>' and 'infix:<&&>' can only be optimized here because we know we want "bool"
            if (  $cond->code eq 'infix:<&&>'
               || $cond->code eq 'infix:<and>'
               ) 
            {
                return '(' . to_boolean($cond->{arguments}->[0], $level) . ' && '
                           . to_boolean($cond->{arguments}->[1], $level) . ')'
            }
            if (  $cond->code eq 'infix:<||>'
               || $cond->code eq 'infix:<or>'
               ) 
            {
                return '(' . to_boolean($cond->{arguments}->[0], $level) . ' || '
                           . to_boolean($cond->{arguments}->[1], $level) . ')'
            }
            if (  $cond->code eq 'prefix:<!>'
               || $cond->code eq 'prefix:<not>'
               )
            {
                if (@{$cond->{arguments}} == 1) {
                    return '!' . to_boolean($cond->{arguments}->[0], $level)
                }
            }
            if (  $cond->code eq 'infix:<eq>' ) {
                return       to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.equals('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ')'
            }
            if (  $cond->code eq 'infix:<ne>' ) {
                return '!' . to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.equals('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ')'
            }
            if (  $cond->code eq 'infix:<le>' ) {
                return '(' . to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.compareTo('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ') <= 0)'
            }
            if (  $cond->code eq 'infix:<ge>' ) {
                return '(' . to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.compareTo('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ') >= 0)'
            }
            if (  $cond->code eq 'infix:<lt>' ) {
                return '(' . to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.compareTo('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ') < 0)'
            }
            if (  $cond->code eq 'infix:<gt>' ) {
                return '(' . to_native_str($cond->{arguments}->[0], $level, 'scalar') . '.compareTo('
                           . to_native_str($cond->{arguments}->[1], $level, 'scalar') . ') > 0)'
            }
            if (  $cond->code eq 'defined' ) {
                my $arg = $cond->{arguments}[0];
                if (  ref( $arg ) eq 'Perlito5::AST::Apply' 
                   && $arg->{code} eq 'prefix:<&>'
                   )
                {
                    my $arg2   = $arg->{arguments}->[0];
                    return '!PlV.code_lookup_by_name_no_autoload(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg2->emit_java($level) . ').is_undef()';
                }
                elsif (  ref( $arg ) eq 'Perlito5::AST::Var' 
                   && $arg->{sigil} eq '&'
                   )
                {
                    my $name = $arg->{name};
                    my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                    return '!PlV.cget_no_autoload(' . Perlito5::Java::escape_string($namespace . '::' . $name) . ').is_undef()';
                }
                # TODO - use this code for typed variables:
                #   $invocant . ' != null' 
                return '!' . $arg->emit_java($level, 'scalar') . '.is_undef()';
            }
        }

        if  (  ($class eq 'Perlito5::AST::Int' )
            || ($class eq 'Perlito5::AST::Num' )
            || ($class eq 'Perlito5::AST::Apply'  && exists $op_to_boolean{ $cond->code })
            )
        {
            return $cond->emit_java($level, $wantarray) . '.to_boolean()';
        }
        else {
            return $cond->emit_java($level, $wantarray) . '.to_boolean()';
        }
}

sub is_scalar {
        $_[0]->isa( 'Perlito5::AST::Int' )
     || $_[0]->isa( 'Perlito5::AST::Num' )
     || $_[0]->isa( 'Perlito5::AST::Buf' )
     || Perlito5::AST::Sub::is_anon_sub($_[0])
     || ($_[0]->isa( 'Perlito5::AST::Var' ) && $_[0]->{sigil} eq '$')
     || ($_[0]->isa( 'Perlito5::AST::Apply' ) 
        && (  exists($op_to_scalar{ $_[0]->{code} })
           )
        )
}

sub to_filehandle {
    my ($item, $level) = @_;

    if (ref($item) eq 'Perlito5::AST::Block') {
        # print { $f } @list
        my $stmts = $item->{stmts};
        if ( @$stmts == 1 ) {
            $item = $stmts->[0];
        }
    }

    return      'PerlOp.get_filehandle('
         .        $item->emit_java( $level ) . ', '
         .        Perlito5::Java::escape_string($Perlito5::PKG_NAME)
         .      ')'
}

sub to_method_call_param_list {
    my ($this, $items, $level) = @_;
    my $items = to_list_preprocess( $items );

    if ($this->isa('Perlito5::AST::Var') && $this->{sigil} eq "::") {
        # convert bareword to string
        $this = Perlito5::AST::Buf->new( buf => $this->{namespace} );
    }

    'PlArray.construct_list_of_aliases('
    .   join(', ',
            $this->emit_java($level, 'scalar', 'lvalue'),
            map( $_->emit_java($level, 'list', 'lvalue'), @$items ),
        )
    . ')';
}

sub to_param_list {
    my $items = to_list_preprocess( $_[0] );
    my $level = $_[1];

    if (@$items == 0) {
        return "new PlArray()";
    }

    if (@$items == 1) {
        my $item = $items->[0];
        if ( $item->isa('Perlito5::AST::Apply') && ( $item->code eq 'infix:<..>' ) ) {
            return '(PlArray)(' . $item->emit_java($level, 'list') . ')';
        }
    }

    'PlArray.construct_list_of_aliases('
    .   join(', ', map( $_->emit_java($level, 'list', 'lvalue'), @$items ))
    . ')';
}

sub to_list_for_push {
    my $items = to_list_preprocess( $_[0] );
    my $level = $_[1];

    if (@$items == 0) {
        return "";
    }

    my $item = $items->[0];
    if ( @$items == 1 && $item->isa('Perlito5::AST::Var') && ( $item->sigil eq '@' ) ) {
        return $item->emit_java($level, 'list');
    }

    return join(', ', map( $_->emit_java($level, 'list'), @$items ));
}

sub to_list {
    my $items = to_list_preprocess( $_[0] );
    my $level = $_[1];

    if (@$items == 0) {
        return "new PlArray()";
    }

    my $item = $items->[0];
    if ( @$items == 1 && $item->isa('Perlito5::AST::Var') && ( $item->sigil eq '@' ) ) {
        return $item->emit_java($level, 'list');
    }

    return 'new PlArray('
         .   join(', ', map( $_->emit_java($level, 'list'), @$items ))
         . ')';
}

sub to_list_preprocess {
    my @items;
    for my $item ( @{$_[0]} ) {
        if (  $item->isa( 'Perlito5::AST::Apply' ) 
           && ( $item->code eq 'circumfix:<( )>' || $item->code eq 'list:<,>' || $item->code eq 'infix:<=>>' )
           )
        {
            if ($item->isa('Perlito5::AST::Apply')
               && $item->code eq 'infix:<=>>'
               )
            {
                $item->{arguments}[0] = Perlito5::AST::Lookup->autoquote( $item->{arguments}[0] );
            }

            for my $arg ( @{ to_list_preprocess($item->arguments) } ) {
                push( @items, $arg);
            }
        }
        else {
            push( @items, $item);
        }
    }
    return \@items;
}

sub to_scalar {
    my $items = to_scalar_preprocess( $_[0] );
    my $level = $_[1];
    my $wantarray = 'scalar';

    # Note: v = 1,2,5  // 5

    @$items
    ?   '('
      .   join(', ', map( $_->emit_java($level, $wantarray), @$items ))
      . ')'
    : 'null'
}

sub to_scalar_preprocess {
    my @items;
    for my $item ( @{$_[0]} ) {
        if (  $item->isa( 'Perlito5::AST::Apply' ) 
           && ( $item->code eq 'list:<,>' || $item->code eq 'infix:<=>>' )
           )
        {
            if ($item->isa('Perlito5::AST::Apply')
               && $item->code eq 'infix:<=>>'
               )
            {
                $item->{arguments}[0] = Perlito5::AST::Lookup->autoquote( $item->{arguments}[0] );
            }

            for my $arg ( @{ to_scalar_preprocess($item->arguments) } ) {
                push( @items, $arg);
            }
        }
        else {
            push( @items, $item);
        }
    }
    return \@items;
}

sub to_runtime_context {
    my $items = to_scalar_preprocess( $_[0] );
    my $level = $_[1];
    my $wantarray = $_[2];   # 'return';

    my @s = grep {$_ ne ''} map( $_->emit_java($level, $wantarray), @$items );

    return 'PerlOp.context(want)'
        if @s == 0;

    return $s[0]
        if @s == 1 && is_scalar($items->[0]);


    if ( @s == 1 && $items->[0]->isa( 'Perlito5::AST::Apply' ) 
       && ( $items->[0]->{code} && $items->[0]->{namespace} )
       )
    {
        # this looks like a plain-perl subroutine call
        return $s[0];
    }

    if ( @s == 1 && $items->[0]->isa( 'Perlito5::AST::Call' ) 
       )
    {
        # TODO - indentify java-native method calls
        # this looks like a plain-perl subroutine call
        return $s[0];
    }

    'PerlOp.context(' . to_context($wantarray) . ', ' 
        .   join(', ', @s)
        . ')'
}

sub to_context {
    my $wantarray = shift;
      $wantarray eq 'list'    ? 'PlCx.LIST' 
    : $wantarray eq 'scalar'  ? 'PlCx.SCALAR' 
    : $wantarray eq 'void'    ? 'PlCx.VOID'
    : $wantarray eq 'statement' ? 'PlCx.VOID'
    : $wantarray eq 'return'  ? 'return_context'
    : $wantarray eq 'runtime' ? 'want'
    :                           'want'    # default = 'runtime'
}

sub autoquote {
    my $index = shift;
    my $level = shift;
    $index = Perlito5::AST::Lookup->autoquote($index);
    return to_str($index, $level);
}

sub emit_java_autovivify {
    my $obj = shift;
    my $level = shift;
    my $type = shift;  # 'array'/'hash'

    if (  $obj->isa( 'Perlito5::AST::Index' )
       || $obj->isa( 'Perlito5::AST::Lookup' )
       || $obj->isa( 'Perlito5::AST::Call' )
       )
    {
        return $obj->emit_java($level, 0, $type);
    }

    # if ( $obj->isa( 'Perlito5::AST::Apply' ) && $obj->code eq 'prefix:<$>' ) {
    #     my $arg  = $obj->{arguments}->[0];
    #     return 'get_scalarref(' 
    #             . $arg->emit_java( $level ) . ', '
    #             . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ', '
    #             . Perlito5::Java::escape_string($type)      # autovivification type
    #             . ')';
    # }

    $obj->emit_java($level)
}

sub emit_java_list_with_tabs {
    my ($level, $argument) = @_;
    my $tab = Perlito5::Java::tab($level);
    return map { ref($_) eq 'ARRAY'
                 ? emit_java_list_with_tabs($level+1, $_)
                 : $tab . $_
               }
               @$argument;
}

sub emit_wrap_java {
    my ($level, @argument) = @_;
    return @argument if wantarray;  # nested calls will execute the formatting only once
    my $s;
    $s = shift @argument if !ref($argument[0]);
    return join("\n", ($s ? $s : ()),
                      emit_java_list_with_tabs($level, [
                            \@argument, 
                      ]));
}

sub emit_wrap_statement_java {
    my ($level, $wantarray, $argument) = @_;
    if ($wantarray eq 'void' || $wantarray eq 'statement') {
        return $argument;
    }
    emit_wrap_java( $level, $argument )
}

sub emit_wrap_last_exception_java {
    my ($self, $stmts, $wantarray) = @_;
    my $block_label = Perlito5::Java::get_java_loop_label( $self->{label} );
    my $test_label = 'e.label_id != 0';
    $test_label = "e.label_id != $block_label && e.label_id != 0"
        if $block_label;
    my @str = (
         "try {",
            [ @$stmts ],
         '}',
         'catch(PlLastException e) {',
            [ "if ($test_label) {",
                 [ "throw e;" ],
              "}"
            ],
         '}' );
    if ($wantarray ne 'void' && $wantarray ne 'statement') {
        push @str, "return PlCx.UNDEF;";
    }
    return @str;
}

1;

=begin

=head1 NAME

Perlito5::Java - Code generator for Perlito Perl5-in-Java

=head1 SYNOPSIS

    $ast->emit_java($level)  # generated Perl5 code

=head1 DESCRIPTION

This module generates Java code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

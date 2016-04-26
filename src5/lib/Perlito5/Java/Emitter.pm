use v5;

use Perlito5::AST;
use Perlito5::AST::Captures;
use Perlito5::Dumper;
use strict;

package Perlito5::Java;
{
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
            perl_to_java        => 'to_long',
            perl_package        => 'Integer',
        };
        $Java_class->{Boolean} = {
            java_type           => 'Boolean',
            java_native_to_perl => 'PlBool',
            perl_to_java        => 'to_bool',
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
        # TODO
        # $Java_class->{Object} = {
        #     java_type           => 'Object',
        #     java_native_to_perl => 'PlObject',
        #     perl_to_java        => 'to_object',
        #     perl_package        => 'Object',
        # };
        $Java_class->{Character} = {
            java_type           => 'Character',
            java_native_to_perl => 'PlString',
            perl_to_java        => 'to_character',
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
            perl_to_java        => 'to_long',
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

    # prefix operators that take a "str" parameter
    our %op_prefix_js_str = (
        'prefix:<-A>' => 'p5atime',
        'prefix:<-C>' => 'p5ctime',
        'prefix:<-M>' => 'p5mtime',
        'prefix:<-d>' => 'p5is_directory',
        'prefix:<-e>' => 'p5file_exists',
        'prefix:<-f>' => 'p5is_file',
        'prefix:<-s>' => 'p5size',
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
    our %op_to_bool = map +($_ => 1), qw(
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
        infix:<->
        infix:<+>
        infix:<*>
        infix:</>
        infix:<%>
        infix:<**>
        infix:<|>
        infix:<&>
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
    our %native_op_to_bool = qw(
        infix:<!=>  !=
        infix:<==>  ==
        infix:<<=>  <=
        infix:<>=>  >=
        infix:<>>   >
        infix:<<>   <
    );
    our %valid_java_statement = qw(
        print           1
        say             1
        printf          1
        infix:<=>       1
        postfix:<++>    1
        postfix:<-->    1
        prefix:<++>     1
        prefix:<-->     1
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
            else {
                push @out, "\"$tmp\"" if $tmp ne '';
                push @out, "(char)" . ord($c) . "";
                $tmp = '';
            }
        }
        push @out, "\"$tmp\"" if $tmp ne '';
        unshift @out, '""' if scalar(@out) > 1;
        return join(' + ', @out);
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
        if ($is_apply && exists $native_op_to_bool{ $self->{code} } && is_native_args($self->{arguments})) {
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
                return '(' . $cond->{int} . ' != 0)';
            }
            elsif ($cond->isa( 'Perlito5::AST::Num' )) {
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
    sub to_bool {
            my $cond = shift;
            my $level = shift;
            my $wantarray = 'scalar';

            if (  $cond->isa( 'Perlito5::AST::Apply' ) && $cond->code eq 'circumfix:<( )>'
               && $cond->{arguments} && @{$cond->{arguments}}
               ) 
            {
                return to_bool( $cond->{arguments}[0], $level )
            }

            # Note: 'infix:<||>' and 'infix:<&&>' can only be optimized here because we know we want "bool"
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<&&>'
                  || $cond->code eq 'infix:<and>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{arguments}->[0], $level) . ' && '
                           . to_bool($cond->{arguments}->[1], $level) . ')'
            }
            if (  $cond->isa( 'Perlito5::AST::Apply' ) 
               && (  $cond->code eq 'infix:<||>'
                  || $cond->code eq 'infix:<or>'
                  )
               ) 
            {
                return '(' . to_bool($cond->{arguments}->[0], $level) . ' || '
                           . to_bool($cond->{arguments}->[1], $level) . ')'
            }

            if  (  ($cond->isa( 'Perlito5::AST::Int' ))
                || ($cond->isa( 'Perlito5::AST::Num' ))
                || ($cond->isa( 'Perlito5::AST::Apply' ) && exists $op_to_bool{ $cond->code })
                )
            {
                return $cond->emit_java($level, $wantarray) . '.to_bool()';
            }
            else {
                return $cond->emit_java($level, $wantarray) . '.to_bool()';
            }
    }

    sub is_scalar {
            $_[0]->isa( 'Perlito5::AST::Int' )
         || $_[0]->isa( 'Perlito5::AST::Num' )
         || $_[0]->isa( 'Perlito5::AST::Buf' )
         || $_[0]->isa( 'Perlito5::AST::Sub' )
         || ($_[0]->isa( 'Perlito5::AST::Var' ) && $_[0]->{sigil} eq '$')
         || ($_[0]->isa( 'Perlito5::AST::Apply' ) 
            && (  exists($op_to_str{ $_[0]->{code} })
               || exists($op_to_num{ $_[0]->{code} })
               || exists($op_to_bool{ $_[0]->{code} })
               #  || $_[0]->{code} eq 'prefix:<\\>'    -- \(@a) is a list
               )
            )
    }

    sub to_list {
        my $items = to_list_preprocess( $_[0] );
        my $level = $_[1];
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
        my $wantarray = 'runtime';

        return $items->[0]->emit_java($level, $wantarray)
            if @$items == 1 && is_scalar($items->[0]);

        'PerlOp.context(want, ' 
            .   join(', ', map( $_->emit_java($level, $wantarray), @$items ))
            . ')'
    }

    sub to_context {
        my $wantarray = shift;
         $wantarray eq 'list'   ? 'PlCx.LIST' 
        :$wantarray eq 'scalar' ? 'PlCx.SCALAR' 
        :$wantarray eq 'void'   ? 'PlCx.VOID'
        :                         'want'
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

        if ( $obj->isa( 'Perlito5::AST::Apply' ) && $obj->code eq 'prefix:<$>' ) {
            my $arg  = $obj->{arguments}->[0];
            return 'get_scalarref(' 
                    . $arg->emit_java( $level ) . ', '
                    . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ', '
                    . Perlito5::Java::escape_string($type)      # autovivification type
                    . ')';
        }

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
        if ($wantarray eq 'void') {
            return $argument;
        }
        emit_wrap_java( $level, $argument )
    }

    sub emit_wrap_last_exception_java {
        my ($self, $stmts) = @_;
        my $block_label = Perlito5::Java::get_java_loop_label( $self->{label} );
        my $test_label = 'e.label_id != 0';
        $test_label = "e.label_id != $block_label && e.label_id != 0"
            if $block_label;
        return (
             "try {",
                [ @$stmts ],
             '}',
             'catch(PlLastException e) {',
                [ "if ($test_label) {",
                     [ "throw e;" ],
                  "}"
                ],
             '}' );
    }

}

package Perlito5::Java::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{block} }
    # top_level - true if this is the main block in a subroutine;
    # create_context - ... 

    sub has_decl {
        my $self = $_[0];
        my $type = $_[1];
        for my $decl ( @{$self->{block}} ) {
            return 1
                if grep { $_->{decl} eq $type } $decl->emit_java_get_decl();
        }
        return 0;
    }

    sub emit_return {
        my ($has_local, $local_label, $value) = @_;
          $has_local
        ? 'return PerlOp.cleanup_local(' . $local_label . ', ' . $value . ')'
        : 'return ' . $value;
    }

    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $original_level = $level;
        my $block_label = Perlito5::Java::get_java_loop_label( $self->{block_label} );
        $Perlito5::THROW = 1 if $block_label;

        my @block;
        for my $stmt (@{$self->{block}}) {
            if (defined($stmt)) {
                if ( ref($stmt) eq 'Perlito5::AST::Apply' && $stmt->code eq 'undef' && !@{$stmt->{arguments}} ) {
                    # don't emit code
                }
                else {
                    push @block, $stmt;
                }
            }
        }
        if ($self->{top_level} && !@block) {
            push @block, Perlito5::AST::Apply->new( code => 'return', arguments => [] );
        }
        my @str;
        my @pre;
        my $has_local = $self->has_decl("local");
        my $has_regex = 0;
        if (grep {$_->emit_java_has_regex()} @block) {
            # regex variables like '$1' are implicitly 'local'
            # $has_local = 1; # TODO: fix, temporarily turn it off
            $has_regex = 1;
        }

        my $local_label = Perlito5::Java::get_label();
        # $has_local = 0;

        if ( $has_local ) {
            push @pre, 'int ' . $local_label . ' = PerlOp.local_length();';

            # TODO
            # push @pre, (
            #         ( $has_regex
            #           ? ( 'var regex_tmp = p5_regex_capture;',
            #               'p5LOCAL.push(function(){ p5_regex_capture = regex_tmp });',
            #           )
            #           : ()
            #         )
            #     );
        }

        my $create_context = $self->{create_context} && $self->has_decl("my");
        my $outer_pkg   = $Perlito5::PKG_NAME;

        if ($self->{top_level} || $create_context) {
            $level++;
        }

        my $last_statement;
        if ($wantarray ne 'void') {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
                $Perlito5::PKG_NAME = $decl->{namespace};
            }

            my @var_decl = $decl->emit_java_get_decl();
            for my $arg (@var_decl) {
                push @str, $arg->emit_java_init($level, $wantarray);
            }

            if ( !( $decl->isa('Perlito5::AST::Decl') && ($decl->decl eq 'my' || $decl->decl eq 'our') ) ) {
                if (  ( $decl->isa('Perlito5::AST::Int') )
                   || ( $decl->isa('Perlito5::AST::Num') )
                   || ( $decl->isa('Perlito5::AST::Buf') )
                   )
                {
                    # this looks like dead code
                }
                elsif ( $decl->isa('Perlito5::AST::Apply')
                  && !( $decl->{namespace} eq 'Java' && $decl->{code} eq 'inline' ) 
                  && !( $Perlito5::Java::valid_java_statement{ $decl->{code} } ) 
                  )
                {
                    # workaround for "Error: not a statement"
                    push @str, 'PerlOp.statement(' . $decl->emit_java( $level, 'void' ) . ');';
                }
                elsif ( $decl->isa('Perlito5::AST::CompUnit')
                      || $decl->isa('Perlito5::AST::For' )
                      || $decl->isa('Perlito5::AST::While' )
                      || $decl->isa('Perlito5::AST::If' )
                      || $decl->isa('Perlito5::AST::Block' )
                      )
                {
                    push @str, $decl->emit_java( $level, 'void' );
                }
                else {
                    push @str, $decl->emit_java( $level, 'void' ) . ';';
                }
            }
        }

        if ($last_statement) {

            my @var_decl = $last_statement->emit_java_get_decl();
            for my $arg (@var_decl) {
                push @str, $arg->emit_java_init($level, $wantarray);
            }

            # if  (  $last_statement->isa( 'Perlito5::AST::Apply' ) 
            #     && $last_statement->code eq 'return'
            #     && $self->{top_level}
            #     && @{ $last_statement->{arguments} }
            #     ) 
            # {
            #     $last_statement = $last_statement->{arguments}[0];
            # }

            if    (  $last_statement->isa( 'Perlito5::AST::For' )
                  || $last_statement->isa( 'Perlito5::AST::While' )
                  || $last_statement->isa( 'Perlito5::AST::Block' )
                  || $last_statement->isa( 'Perlito5::AST::Use' )
                  || $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'goto'
                  )
            {
                push @str, $last_statement->emit_java($level, 'void') . ';';
                push @str, emit_return($has_local, $local_label, 'PerlOp.context(want)') . ';'; 
            }
            elsif ( $last_statement->isa( 'Perlito5::AST::If' ) ) {
                push @str, $last_statement->emit_java($level, 'runtime') . '';
                # push @str, 'return PlCx.UNDEF;';  # unreachable
            }
            else {
                if ( $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return' ) {

                    if ( $self->{top_level} ) {
                        if (!@{$last_statement->{arguments}}) {
                            push @str, emit_return($has_local, $local_label, 'PerlOp.context(want)') . ';'; 
                        }
                        else {
                            push @str, emit_return($has_local, $local_label,
                                    $wantarray eq 'runtime'
                                  ? Perlito5::Java::to_runtime_context([$last_statement->{arguments}[0]], $level+1)
                                  : $wantarray eq 'scalar'
                                  ? Perlito5::Java::to_scalar([$last_statement->{arguments}[0]], $level+1)
                                  : $last_statement->{arguments}[0]->emit_java($level, $wantarray)
                                ) . ';';
                        }
                    }
                    else {
                        if (!@{$last_statement->{arguments}}) {
                            $Perlito5::THROW_RETURN = 1;
                            push @str, 'return PerlOp.ret(PerlOp.context(want));'; 
                        }
                        else {
                            $Perlito5::THROW_RETURN = 1;
                            push @str, 'return PerlOp.ret('
                                . Perlito5::Java::to_runtime_context([$last_statement->{arguments}[0]], $level+1)
                                . ');';
                        }
                    }
                }
                else {
                    push @str, emit_return($has_local, $local_label,
                            $wantarray eq 'runtime'
                          ? Perlito5::Java::to_runtime_context([$last_statement], $level+1)
                          : $wantarray eq 'scalar'
                          ? Perlito5::Java::to_scalar([$last_statement], $level+1)
                          : $last_statement->emit_java($level, $wantarray)
                    ) . ';';
                }
            }
        }
        my $out;

        if ($self->{eval_block}) {
            return ( @pre,
                "try {",
                    \@str,
                "}",
                "catch(PlReturnException e) {",
                    [ emit_return($has_local, $local_label, 'e.ret') . ";" ],
                "}",
                "catch(PlNextException e) {",
                    [ "throw e;" ],
                "}",
                "catch(PlLastException e) {",
                    [ "throw e;" ],
                "}",
                "catch(PlRedoException e) {",
                    [ "throw e;" ],
                "}",
                "catch(PlDieException e) {",
                    [ 'PlV.set("main::v_@", e.ret);',
                      "return PlCx.UNDEF;",
                    ],
                "}",
                "catch(Exception e) {",
                    [ 'PlV.set("main::v_@", new PlString(e.getMessage()));',
                      "return PlCx.UNDEF;",
                    ],
                "}",
            );
        }
        elsif ($self->{top_level} && $Perlito5::THROW_RETURN) {
            push @pre,
                "try {",
                   [ @str ],
                '}',
                'catch(PlReturnException e) {',
                   [ emit_return($has_local, $local_label, 'e.ret') . ";" ],
                '}';
            @str = ();
        }
        elsif ( ($Perlito5::THROW || ($self->{continue} && @{$self->{continue}{stmts}} > 0)) && !$self->{not_a_loop} ) {

            # TODO - emit error message if catched a "next/redo/last LABEL" when expecting a "return" exception

            my $redo_label = Perlito5::Java::get_label();
            my $test_label = 'e.label_id != 0';
            $test_label = "e.label_id != $block_label && e.label_id != 0"
                if $block_label;

            my @continue;
            if ($self->{continue}) {

                # TODO - set up next/last/redo inside continue block

                push @continue, 
                    "if (!$redo_label) {",
                      [ "try {",
                           [ Perlito5::Java::LexicalBlock->new(
                               block => $self->{continue}{stmts},
                               not_a_loop => 1,
                             )->emit_java($level + 2, $wantarray)
                           ],
                        '}',
                        'catch(PlNextException e) {',
                           [ "if ($test_label) {",
                                [ "throw e;" ],
                             "}"
                           ],
                        '}',
                        'catch(PlRedoException e) {',
                           [ "if ($test_label) {",
                                [ "throw e;" ],
                             "}",
                             "$redo_label = true;",
                           ],
                        '}',
                      ],
                    "}";
            }

            push @pre,
                "boolean $redo_label;",
                "do {",
                  [
                    "$redo_label = false;",
                    "try {",
                       [ @str ],
                    '}',
                    'catch(PlNextException e) {',
                       [ "if ($test_label) {",
                            [ "throw e;" ],
                         "}"
                       ],
                    '}',
                    'catch(PlRedoException e) {',
                       [ "if ($test_label) {",
                            [ "throw e;" ],
                         "}",
                         "$redo_label = true;",
                       ],
                    '}',
                    @continue,
                  ],
                "} while ($redo_label);";
            @str = ();
        }
        else {
            if ($has_local) {
                push @str, 'PerlOp.cleanup_local(' . $local_label . ', PlCx.UNDEF);';
            }
        }
        $Perlito5::PKG_NAME = $outer_pkg;
        return ( @pre, @str );
    }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::CompUnit;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        return Perlito5::Java::LexicalBlock->new(
                block => $self->{body},
                not_a_loop => 1,
            )->emit_java( $level + 1, $wantarray );
    }
    sub process_java_import_statement {
        my ($unit_stmt) = @_;

        # Perl:
        #   package Put { import => 'java.Put' };
        #
        # AST:
        #   bless({
        #       'stmts' => [
        #           bless({
        #               'arguments' => [],
        #               'code' => 'package',
        #               'namespace' => 'Put',
        #           }, 'Perlito5::AST::Apply'),
        #           bless({
        #               'arguments' => [
        #                   bless({
        #                       'arguments' => [],
        #                       'bareword' => 1,
        #                       'code' => 'import',
        #                       'namespace' => '',
        #                   }, 'Perlito5::AST::Apply'),
        #                   bless({
        #                       'buf' => 'java.Put',
        #                   }, 'Perlito5::AST::Buf'),
        #               ],
        #               'code' => 'infix:<=>>',
        #               'namespace' => '',
        #           }, 'Perlito5::AST::Apply'),
        #       ],
        #   }, 'Perlito5::AST::Block'),

        my $str = '';
        if ( ref($unit_stmt) eq 'Perlito5::AST::Block') {

            my $stmt = $unit_stmt->{stmts} // [];

            return '' if @$stmt != 2;    # exactly 2 statements: "package" + "options"
            return '' unless ($stmt->[0] && ref($stmt->[0]) eq 'Perlito5::AST::Apply' && $stmt->[0]->{code} eq 'package');
            return '' unless ($stmt->[1] && ref($stmt->[1]) eq 'Perlito5::AST::Apply' && ( $stmt->[1]->{code} eq 'infix:<=>>' || $stmt->[1]->{code} eq 'list:<,>'));

            my $Java_class = Perlito5::Java::get_java_class_info();
            my $class = $stmt->[0]->{namespace};
            # warn "Java class: $class\n";
            # TODO - add more information about the class

            # we need the parameter list as Perl data, so we need to evaluate the AST
            # - wrap the "list AST into a "hashref" AST
            my $args_ast = Perlito5::AST::Apply->new(
                arguments => [ $stmt->[1] ],
                code => 'circumfix:<{ }>',
            );
            # - transform the AST back into Perl code
            my $out = [];
            Perlito5::Perl5::PrettyPrinter::pretty_print( [$args_ast->emit_perl5()], 0, $out );
            my $args_perl5 = join( '', @$out );

            # - eval the Perl code and store the arguments to use later
            $Java_class->{$class} = eval $args_perl5
                or die "error in arguments to generate Java class:\n$@\n${args_perl5}";


            if ($Java_class->{$class}->{java_path}) {
                # package header { java_path => 'org.perlito.udfs' }
                #  ==> $Java_class->{header}->{java_path} = 'org.perlito.udfs';
                $str .= "package $Java_class->{$class}->{java_path};\n";
            }
            elsif ($Java_class->{$class}->{import}) {
                Perlito5::Java::set_java_class_defaults(
                    $class, $Java_class->{$class}->{import},
                );
            }
            elsif ($Java_class->{$class}->{extends}) {
                # extends => 'JavaObject',              # Perl package name (a class imported from Java)
                # methods => [ ... ]

                my $extended = $Java_class->{ $Java_class->{$class}->{extends} };
                if ($extended) {
                    $Java_class->{$class}->{extends_java_type} = $extended->{java_type};

                }
                else {
                    die "cannot extend class '" . $Java_class->{$class}->{extends} . "' because it was not declared";
                }

                my $perl_to_java = $class;
                $perl_to_java =~ s/:://g;
                Perlito5::Java::set_java_class_defaults(
                    $class, $perl_to_java,
                );

                # warn Data::Dumper::Dumper $Java_class->{$class};
                # warn "'extends' not implemented";
            }
            elsif ($Java_class->{$class}->{implements}) {
                # implements => 'JavaObject',              # Perl package name (a class imported from Java)
                # methods => [ ... ]

                my $implemented = $Java_class->{ $Java_class->{$class}->{implements} };
                if ($implemented) {
                    $Java_class->{$class}->{implements_java_type} = $implemented->{java_type};

                }
                else {
                    die "cannot implement class '" . $Java_class->{$class}->{implements} . "' because it was not declared";
                }

                my $perl_to_java = $class;
                $perl_to_java =~ s/:://g;
                Perlito5::Java::set_java_class_defaults(
                    $class, $perl_to_java,
                );

                # warn Data::Dumper::Dumper $Java_class->{$class};
                # warn "'implements' not implemented";
            }
            else {
                die "missing 'import' argument to generate Java class";
            }


            # throw away this block - generate no Perl code
            $unit_stmt->{stmts} = [];

        }
        return $str;
    }
    sub emit_java_program {
        my ($comp_units, %options) = @_;
        $Perlito5::PKG_NAME = 'main';
        $Perlito5::THROW = 0;
        $Perlito5::THROW_RETURN = 0;
        my $level = 0;
        my $wantarray = 'void';
        my $str;
        $str .= Perlito5::Compiler::do_not_edit("//");

        # look for special 'Java' packages
        Perlito5::Java::init_java_class();
        for my $comp_unit ( @$comp_units ) {
            for my $unit_stmt ( @{ $comp_unit->{body} } ) {
                $str .= process_java_import_statement($unit_stmt);

                # if ( ref($unit_stmt) eq 'Perlito5::AST::CompUnit') {
                #     my $stmt = $unit_stmt->{stmts} // [];
                # }

            }
        }

        my @main;
        for my $comp_unit ( @$comp_units ) {
            push @main, $comp_unit->emit_java($level + 1, $wantarray);
        }
        if ($options{'expand_use'}) {
            my $Java_class = Perlito5::Java::get_java_class_info();
            $str .= Perlito5::Java::Runtime->emit_java(
                java_classes => $Java_class,
                java_constants => \@Perlito5::Java::Java_constants,
            );
        }

        $str .= Perlito5::Java::emit_wrap_java(-1,
             "class Main {",
               [ "public static void main(String[] args) {",
                   [ "PlEnv.init(args);",
                     "int want = PlCx.VOID;",
                     "try {",
                       [ @Perlito5::Java::Java_init,
                         @main,
                       ],
                     "}",
                     "catch(PlReturnException e) {",
                         [ 'PlCORE.die("Can\'t return outside a subroutine");' ],
                     "}",
                     "catch(PlNextException e) {",
                         [ 'PlCORE.die("Can\'t \\"next\\" outside a loop block");' ],
                     "}",
                     "catch(PlLastException e) {",
                         [ 'PlCORE.die("Can\'t \\"last\\" outside a loop block");' ],
                     "}",
                     "catch(PlRedoException e) {",
                         [ 'PlCORE.die("Can\'t \\"redo\\" outside a loop block");' ],
                     "}",
                   ],
                 "}",
               ],
               [ "public static void init() {",
                 [
                    "main(new String[]{});"
                 ],
                 "}",
               ],
               [ "public static PlObject[] apply(String functionName, String... args) {",
                 [
                     "PlArray list = new PlArray(args);",
                     "PlObject result = PlV.get(functionName).apply(PlCx.LIST, list);",
                     "PlArray res = result instanceof PlArray ? (PlArray) result : new PlArray(result);",
                     "PlObject[] out = new PlObject[res.to_int()];",
                     "int i = 0;",
                     "for (PlObject s : res.a) {",
                         [ "out[i++] = s;",
                         ],
                     "}",
                     "return out;",
                 ],
                 "}",
               ],
               [ "public static PlObject[] apply(String functionName, PlObject... args) {",
                 [
                     "PlArray list = new PlArray(args);",
                     "PlObject result = PlV.get(functionName).apply(PlCx.LIST, list);",
                     "PlArray res = result instanceof PlArray ? (PlArray) result : new PlArray(result);",
                     "PlObject[] out = new PlObject[res.to_int()];",
                     "int i = 0;",
                     "for (PlObject s : res.a) {",
                         [ "out[i++] = s;",
                         ],
                     "}",
                     "return out;",
                 ],
                 "}",
               ],
             "}",
        ) . "\n";

        return $str;
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Int;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $v = $self->{int};
        if ( $v > (2**63-1) ) {
            return "new PlDouble(" . $v . ".0)";
        }
        if ( $v >= -2 && $v < 0) {
            return "PlCx.MIN" . abs($v);
        }
        if ( $v >= 0 && $v <= 2) {
            return "PlCx.INT" . abs($v);
        }
        "new PlInt(" . $v . "L)";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Num;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        "new PlDouble(" . $self->{num} . ")";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Buf;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        "new PlString(" . Perlito5::Java::escape_string( $self->{buf} ) . ")";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Block;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        local $Perlito5::THROW = 0;
        my $body;
        if ($wantarray ne 'void') {
            $body = Perlito5::Java::LexicalBlock->new( block => $self->{stmts}, block_label => $self->{label},
                continue => $self->{continue},
            );
        }
        else {
            $body = Perlito5::Java::LexicalBlock->new( block => $self->{stmts}, block_label => $self->{label},
                continue => $self->{continue},
            );
        }

        my $init = "";
        if ($self->{name} eq 'INIT') {
            my $tmp  = 'p5pkg.main.' . Perlito5::Java::get_label();

            # INIT-blocks execute only once
            $init = Perlito5::Java::tab($level + 2) . "if ($tmp) { return }; $tmp = 1;\n";

            # TODO - make this execute before anything else

        }

        my @str = $body->emit_java($level + 1, $wantarray);
        if ($Perlito5::THROW) {
            @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str );
        }
        return Perlito5::Java::emit_wrap_java($level, @str);
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Index;
{
    sub emit_java {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $method = $autovivification_type || 'aget';
        $method = 'aget_scalarref' if $autovivification_type eq 'scalar';
        $method = 'aget_arrayref'  if $autovivification_type eq 'array';
        $method = 'aget_hashref'   if $autovivification_type eq 'hash';
        $method = 'aget_lvalue'    if $autovivification_type eq 'lvalue';
        $method = 'aget_lvalue_local' if $autovivification_type eq 'local';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->code eq 'circumfix:<( )>'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            # (4,5,6)[0,2]
            return 'p5list_slice('
                        . $self->{obj}->emit_java($level, 'list') . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::Java::to_context($wantarray)
                   . ')'
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '%'
              )
           )
        {
            # Perl5.20 hash slice
            # %a[10, 20]
            # %$a[0, 2] ==> %{$a}[0,2]

            # "fix" the sigil type
            my $obj = $self->{obj};
            $obj->{sigil} = '@'
                if $obj->{sigil} eq '%';
            $obj->{code} = 'prefix:<@>'
                if $obj->{code} eq 'prefix:<%>';

            return 'p5hash_slice('
                        . $self->{obj}->emit_java($level, 'list') . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::Java::to_context($wantarray)
                   . ')';
        }
        my $arg = $self->{index_exp};
        my $s;
        if ($arg->isa('Perlito5::AST::Int')) {
            $s = $arg->{int};
        }
        else {
            $s = $arg->emit_java($level, 'scalar');
        }
        return $self->emit_java_container($level) . '.' . $method . '(' . $s . ')';
    }
    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray, $localize) = @_;
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            return Perlito5::Java::emit_wrap_java($level, 
                    'var a = [];',
                    'var v = ' . Perlito5::Java::to_list([$self->{index_exp}], $level) . ';',
                    'var src=' . Perlito5::Java::to_list([$arguments], $level) . ";",
                    'var out=' . Perlito5::Java::emit_java_autovivify( $self->{obj}, $level, 'array' ) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i) {',
                          [ 'tmp = src.aget(i);',
                            'out.aset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        if ($localize) {
            return $self->emit_java_container($level) . '.aget_lvalue_local('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ').set('
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')';
        }
        my $arg = $self->{index_exp};
        my $s;
        if ($arg->isa('Perlito5::AST::Int')) {
            $s = $arg->{int};
        }
        else {
            $s = $arg->emit_java($level, 'scalar');
        }
        return $self->emit_java_container($level) . '.aset(' 
                    . $s . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
                . ')';
    }
    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        my $wantarray = 'list';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a[10, 20]
            # @$a[0, 2] ==> @{$a}[0,2]
            return Perlito5::Java::emit_wrap_java($level, 
                    'var a = [];',
                    'var v = ' . Perlito5::Java::to_list([$self->{index_exp}], $level) . ';',
                    'var out=' . Perlito5::Java::emit_java_autovivify( $self->{obj}, $level, 'array' ) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i) {',
                          [ 'tmp = ' . $list . '.shift();',
                            'out.aset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        my $arg = $self->{index_exp};
        my $s;
        if ($arg->isa('Perlito5::AST::Int')) {
            $s = $arg->{int};
        }
        else {
            $s = $arg->emit_java($level, 'scalar');
        }
        return $self->emit_java_container($level) . '.aset(' 
                    . $s . ', '
                    . $list . '.shift()'
                . ')';
    }
    sub emit_java_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # ${"Exporter::Cache"}[2]
            # $$a[0] ==> $a->[0]
            my $v = Perlito5::AST::Apply->new( %{$self->{obj}}, code => 'prefix:<@>' );
            return $v->emit_java($level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->code eq 'circumfix:<( )>'
           )
        {
            # the expression inside () returns a list
            return Perlito5::Java::to_list([$self->{obj}], $level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            $self->{obj}->{sigil} = '@';
            return $self->{obj}->emit_java($level);
        }
        else {
            return Perlito5::Java::emit_java_autovivify( $self->{obj}, $level, 'array' );
        }
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Lookup;
{
    sub emit_java {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $method = $autovivification_type || 'hget';
        $method = 'hget_scalarref' if $autovivification_type eq 'scalar';
        $method = 'hget_arrayref'  if $autovivification_type eq 'array';
        $method = 'hget_hashref'   if $autovivification_type eq 'hash';
        $method = 'hget_lvalue'    if $autovivification_type eq 'lvalue';
        $method = 'hget_lvalue_local' if $autovivification_type eq 'local';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            if ( $self->{obj}->isa('Perlito5::AST::Var') ) {
                $v = $self->{obj};
            }
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');

            return $v->emit_java($level, 'list') . ".$method("
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level)
                    . ')'
        }
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<%>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '%'
              )
           )
        {
            # Perl5.20 hash slice
            # %a{ 'x', 'y' }
            # %$a{ 'x', 'y' }  ==> %{$a}{ 'x', 'y' }
            my $v;
            if ( $self->{obj}->isa('Perlito5::AST::Var') ) {
                $v = $self->{obj};
            }
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');

            return 'p5hash_lookup_slice('
                        . $v->emit_java($level, 'list') . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level) . ', '
                        . Perlito5::Java::to_context($wantarray)
                   . ')'
        }
        return $self->emit_java_container($level) . '.' . $method . '('
                . Perlito5::Java::autoquote($self->{index_exp}, $level)
            . ')';
    }
    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray, $localize) = @_;
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            $v = $self->{obj}
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return $self->emit_java_container($level). '.hset('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . Perlito5::Java::to_list([$arguments], $level) . ', '
                    . Perlito5::Java::to_list([$self->{index_exp}], $level)
                . ')'
        }
        if ($localize) {
            return $self->emit_java_container($level) . '.hget_lvalue_local('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ').set('
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')';
        }
        return $self->emit_java_container($level) . '.hset('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')';
    }
    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        my $wantarray = 'list';
        if (  (  $self->{obj}->isa('Perlito5::AST::Apply')
              && $self->{obj}->{code} eq 'prefix:<@>'
              )
           || (  $self->{obj}->isa('Perlito5::AST::Var')
              && $self->{obj}->sigil eq '@'
              )
           )
        {
            # @a{ 'x', 'y' }
            # @$a{ 'x', 'y' }  ==> @{$a}{ 'x', 'y' }
            my $v;
            $v = $self->{obj}
                if $self->{obj}->isa('Perlito5::AST::Var');
            $v = Perlito5::AST::Apply->new( code => 'prefix:<%>', namespace => $self->{obj}->namespace, arguments => $self->{obj}->arguments )
                if $self->{obj}->isa('Perlito5::AST::Apply');
            return Perlito5::Java::emit_wrap_java($level, 
                    'var a = [];',
                    'var v = ' . Perlito5::Java::to_list([$self->{index_exp}], $level) . ';',
                    'var out=' . $v->emit_java($level) . ";",
                    'var tmp' . ";",
                    'for (var i=0, l=v.length; i<l; ++i)' . '{',
                          [ 'tmp = ' . $list . '.shift();',
                            'out.hset(v[i], tmp);',
                            'a.push(tmp)',
                          ],
                    '}',
                    'return a',
            )
        }
        return $self->emit_java_container($level) . '.hset('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ', '
                    . $list . '.shift()'
            . ')';
    }
    sub emit_java_container {
        my $self = shift;
        my $level = shift;
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # ${"Exporter::Cache"}{x}
            # $$a{0} ==> $a->{0}
            my $v = Perlito5::AST::Apply->new( %{$self->{obj}}, code => 'prefix:<%>' );
            return $v->emit_java($level);
        }
        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->sigil eq '$'
           )
        {
            # my $v = $self->{obj};   HERE

            #if ($self->{obj}{_real_sigil} ne '%') {
            #    warn Data::Dumper::Dumper($self->{obj});
            #}

            my $v = Perlito5::AST::Var->new( %{$self->{obj}}, sigil => '%' );
            return $v->emit_java($level)
        }
        else {
            return Perlito5::Java::emit_java_autovivify( $self->{obj}, $level, 'hash' );
        }
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Var;
{
    my $table = {
        '$' => 'v_',
        '@' => 'List_',
        '%' => 'Hash_',
        '&' => '',
    };

    sub emit_java_global {
        my ($self, $level, $wantarray, $localize) = @_;
        my $local = $localize ? "_local" : "";
        my $str_name = $self->{name};
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $namespace = $self->{namespace} || $self->{_namespace};
        if ($sigil eq '@' && $self->{name} eq '_' && $namespace eq 'main') {
            # XXX - optimization - @_ is a lexical
            my $s = 'List__';
            if ($self->{sigil} eq '$#') {
                return $s . '.end_of_array_index()';
            }
            if ( $wantarray eq 'scalar' ) {
                return $s . '.length_of_array()';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(want == PlCx.LIST'
                    . ' ? ' . $s
                    . ' : ' . $s . '.to_long()'
                    . ')';
            }
            return $s;
        }

        if ($sigil eq '$' && $self->{name} > 0) {
            # regex captures
            return 'p5_regex_capture[' . ($self->{name} - 1) . ']'
        }
        if ( $sigil eq '::' ) {
            return Perlito5::Java::escape_string( $namespace );
        }

        my $index = Perlito5::Java::escape_string($namespace . '::' . $table->{$sigil} . $str_name);
        if ( $sigil eq '$' ) {
            return "PlV.get$local(" . $index . ')';
        }
        if ( $sigil eq '*' ) {
        }
        if ( $sigil eq '&' ) {
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            return 'PlV.get(' . Perlito5::Java::escape_string($namespace . '::' . $str_name ) . ')'
                . '.apply(' . Perlito5::Java::to_context($wantarray) . ', List__)';
        }
        if ($sigil eq '@') {
            if ($self->{sigil} eq '$#') {
                return "PlV.array_get$local(" . $index . ').end_of_array_index()'
            }
            my $s = "PlV.array_get$local(" . $index . ')';
            if ( $wantarray eq 'scalar' ) {
                return $s . '.length_of_array()';
            }
            return $s;
        }
        if ($sigil eq '%') {
            return "PlV.hash_get$local(" . $index . ')';
        }
        die "don't know how to access variable ", $sigil, $self->name;
    }

    sub emit_java_global_set {
        my ($self, $arguments, $level, $wantarray, $localize) = @_;
        my $local = $localize ? "_local" : "";
        my $str_name = $self->{name};
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $namespace = $self->{namespace} || $self->{_namespace};
        if ($sigil eq '@' && $self->{name} eq '_' && $namespace eq 'main') {
            # XXX - optimization - @_ is a lexical
            my $s = 'List__';
            if ($self->{sigil} eq '$#') {
                return $s . '.set_end_of_array_index(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')';
            }
            if ( $wantarray eq 'scalar' ) {
                return $s . '.to_long()';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(want'
                    . ' ? ' . $s
                    . ' : ' . $s . '.to_long()'
                    . ')';
            }
            return $s;
        }

        if ($sigil eq '$' && $self->{name} > 0) {
            # regex captures
            return 'p5_regex_capture[' . ($self->{name} - 1) . ']'
        }
        if ( $sigil eq '::' ) {
            return Perlito5::Java::escape_string( $namespace );
        }

        my $index = Perlito5::Java::escape_string($namespace . '::' . $table->{$sigil} . $str_name);
        if ( $sigil eq '$' ) {
            return "PlV.set$local(" . $index . ', ' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')';
        }
        if ( $sigil eq '@' ) {

            if ($self->{sigil} eq '$#') {
                $self->{sigil} = '@';
                return 'PlV.array_get(' . $index . ').set_end_of_array_index(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')';
            }
            # TODO - return in the right context
            return "PlV.array_set$local(" . $index . ', ' . Perlito5::Java::to_list([$arguments], $level+1) . ')';
        }
        if ( $sigil eq '%' ) {
            return "PlV.hash_set$local(" . $index . ', ' . Perlito5::Java::to_list([$arguments], $level+1) . ')';
        }
        if ( $sigil eq '*' ) {
            die "don't know how to assign to variable ", $sigil, $self->name;
        }
        if ( $sigil eq '&' ) {
            # return 'PlV.get(' . $index . ').apply(' . Perlito5::Java::to_context($wantarray) . ', List__)';
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $decl_type = $self->{_decl} || 'global';
        if ( $decl_type ne 'my' && $decl_type ne 'our' ) {
            return $self->emit_java_global($level, $wantarray);
        }
        my $str_name = $table->{$sigil} . $self->{name} . "_" . $self->{_id};

        $str_name = $Perlito5::Java::Java_var_name{$self->{_id}}
            if exists $Perlito5::Java::Java_var_name{$self->{_id}};

        if ( $sigil eq '@' ) {
            if ( $wantarray eq 'scalar' ) {
                return $self->emit_java($level, 'list') . '.length_of_array()';
            }
            if ( $wantarray eq 'runtime' ) {
                return '(want == PlCx.LIST'
                    . ' ? ' . $self->emit_java($level, 'list')
                    . ' : ' . $self->emit_java($level, 'list') . '.length_of_array()'
                    . ')';
            }
        }
        if ($self->{sigil} eq '$#') {
            return $str_name . '.end_of_array_index()';
        }
        return $str_name;
    }

    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $decl_type = $self->{_decl} || 'global';
        if ( $decl_type ne 'my' && $decl_type ne 'our' ) {
            return $self->emit_java_global_set($arguments, $level, $wantarray);
        }
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        if ( $sigil eq '$' ) {
            my $id = $self->{_id};
            my $Java_var = Perlito5::Java::get_java_var_info();
            my $type = $Java_var->{ $id }{type} || 'PlLvalue';
            if ($type ne 'PlLvalue') {
                # set a typed variable - there is no .set() method
                # the arguments are not boxed
                return $self->emit_java() . ' = ' . Perlito5::Java::to_native_args([$arguments]);
            }
            return $self->emit_java() . '.set(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')'
        }
        if ( $sigil eq '@' ) {

            if ($self->{sigil} eq '$#') {
                $self->{sigil} = '@';
                return $open . $self->emit_java() . '.set_end_of_array_index(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')' . $close
            }

            return $self->emit_java() . '.set(' . Perlito5::Java::to_list([$arguments], $level+1) . ')'
        }
        if ( $sigil eq '%' ) {
            return $self->emit_java() . '.set(' . Perlito5::Java::to_list([$arguments], $level+1, 'hash') . ')'
        }
        if ( $sigil eq '*' ) {
            my $namespace = $self->{namespace} || $self->{_namespace};
            return 'p5typeglob_set(' 
            .   Perlito5::Java::escape_string($namespace) . ', '
            .   Perlito5::Java::escape_string($self->{name}) . ', ' 
            .   Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')'
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        if ( $sigil eq '$' ) {
            return $self->emit_java() . '.set(' . $list  . '.shift())'
        }
        if ( $sigil eq '@' ) {
            return join( ";\n" . Perlito5::Java::tab($level),
                $self->emit_java() . ' = ' . $list,
                $list . ' = []'
            );
        }
        if ( $sigil eq '%' ) {
            return join( ";\n" . Perlito5::Java::tab($level),
                $self->emit_java() . ' = new PlHash(' . $list  . ')',
                $list . ' = []'
            );
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Decl;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;

        my $var = $self->{var};
        my $localize = '';
        if ($self->{decl} eq 'local') {
            $localize = 'local';
            if ( ref($var) eq 'Perlito5::AST::Var' ) {
                return $var->emit_java_global($level, $wantarray, $localize);
            }
        }
        $var->emit_java( $level, $wantarray, $localize );
    }
    sub emit_java_init {
        my ($self, $level, $wantarray) = @_;

        my $var = $self->{var};
        my $Java_var = Perlito5::Java::get_java_var_info();
        my $type = $self->{type} || 'PlLvalue';
        my $id = $self->{var}{_id};
        if ( $id ) {
            $Java_var->{ $id } = { id => $id, type => $type };
        }

        if ($self->{decl} eq 'local') {
            return '';
        }
        if ($self->{decl} eq 'my') {
            if ($self->{var}->sigil eq '%') {
                return 'PlHash ' . $self->{var}->emit_java() . ' = new PlHash();';
            }
            elsif ($self->{var}->sigil eq '@') {
                return 'PlArray ' . $self->{var}->emit_java() . ' = new PlArray();';
            }
            else {
                my $Java_class = Perlito5::Java::get_java_class_info();
                my $java_type = $Java_class->{$type}{java_type} || 'PlLvalue';
                if( $java_type eq 'PlLvalue' ) {
                    return "${java_type} " . $self->{var}->emit_java() . " = new ${java_type}();";
                } else {
                    return "${java_type} " . $self->{var}->emit_java() . ";";
                }
            }
        }
        elsif ($self->{decl} eq 'our') {
            my $v = Perlito5::AST::Var->new( %{$self->{var}}, _decl => 'my' );
            if ($self->{var}->sigil eq '%') {
                return 'PlHash ' . $v->emit_java() . ' = ' . Perlito5::AST::Var::emit_java_global($self->{'var'}) . ";";
            }
            elsif ($self->{var}->sigil eq '@') {
                return 'PlArray ' . $v->emit_java() . ' = ' . Perlito5::AST::Var::emit_java_global($self->{'var'}) . ";";
            }
            else {
                return 'PlLvalue ' . $v->emit_java() . ' = ' . Perlito5::AST::Var::emit_java_global($self->{'var'}) . ";";
            }
        }
        elsif ($self->{decl} eq 'state') {
            # TODO
            return '// state ' . $self->{var}->emit_java();
        }
        else {
            die "not implemented: Perlito5::AST::Decl '" . $self->{decl} . "'";
        }
    }
    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $var = $self->{var};
        my $localize = '';
        if ($self->{decl} eq 'local') {
            $localize = 'local';
            if ( ref($var) eq 'Perlito5::AST::Var' ) {
                return $var->emit_java_global_set($arguments, $level, $wantarray, $localize);
            }
        }
        $var->emit_java_set($arguments, $level, $wantarray, $localize);
    }
    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        $self->var->emit_java_set_list($level, $list);
    }
    sub emit_java_get_decl {
        my $self = shift;
        return ($self);
    }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Call;
{
    sub emit_java {
        my ($self, $level, $wantarray, $autovivification_type) = @_;
        # autovivification_type: array, hash
        my $meth = $self->{method};

        if ( $meth eq 'postcircumfix:<[ ]>' ) {
            my $method = $autovivification_type || 'aget';
            $method = 'aget_scalarref' if $autovivification_type eq 'scalar';
            $method = 'aget_arrayref'  if $autovivification_type eq 'array';
            $method = 'aget_hashref'   if $autovivification_type eq 'hash';
            $method = 'aget_lvalue'    if $autovivification_type eq 'lvalue';
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'array' )
                . '.' . $method . '(' . Perlito5::Java::to_num($self->{arguments}, $level+1)
                . ')';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            my $method = $autovivification_type || 'hget';
            $method = 'hget_scalarref' if $autovivification_type eq 'scalar';
            $method = 'hget_arrayref'  if $autovivification_type eq 'array';
            $method = 'hget_hashref'   if $autovivification_type eq 'hash';
            $method = 'hget_lvalue'    if $autovivification_type eq 'lvalue';
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'hash' )
                . '.' . $method . '(' . Perlito5::Java::autoquote($self->{arguments}, $level+1, 'list')
                . ')';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            if (  ref( $self->{invocant} ) eq 'Perlito5::AST::Var'
               && $self->{invocant}{sigil} eq '&'
               )
            {
                # &x()
                my $namespace = $self->{invocant}{namespace} || $Perlito5::PKG_NAME;
                return 'PlV.get(' . Perlito5::Java::escape_string($namespace . '::' . $$self->{invocant}{name} ) . ')'
                    . '.apply('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list($self->{arguments})
                    . ')';
            }
            # $x->()
            my $invocant;
            if (  ref( $self->{invocant} ) eq 'Perlito5::AST::Apply' 
               && $self->{invocant}{code} eq 'prefix:<&>'
               )
            {
                my $arg   = $self->{invocant}{arguments}->[0];
                $invocant = 'p5code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_java($level) . ')';
            }
            elsif (  ref( $self->{invocant} ) eq 'Perlito5::AST::Var' 
               && $self->{invocant}{sigil} eq '&'
               )
            {
                $invocant = 'p5pkg[' . Perlito5::Java::escape_string(($self->{invocant}{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::Java::escape_string($self->{invocant}{name} ) . ']';
            }
            elsif (  ref( $self->{invocant} ) eq 'Perlito5::AST::Var' 
               && $self->{invocant}{sigil} eq '::'
               && $self->{invocant}{namespace} eq '__SUB__'
               )
            {
                # __SUB__->()
                $invocant = 'this';     # "this" is the closure
            }
            else {
                $invocant = $self->{invocant}->emit_java($level, 'scalar');
            }

            return $invocant . '.apply('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list($self->{arguments})
                    . ')';
        }

        # class method call in native 'Java' packages
        #
        #   package Sample { import => "misc.Java.Sample" };
        #   Sample->new();  
        #   new Sample();
        #
        if ( ref($self->{invocant}) eq 'Perlito5::AST::Var' && $self->{invocant}->{sigil} eq '::' ) {
            my $Java_class = Perlito5::Java::get_java_class_info();
            if ( exists $Java_class->{$self->{invocant}->{namespace}} ) {
                my $info = $Java_class->{$self->{invocant}->{namespace}};
                if ($meth eq 'new') {
                    return "new $info->{java_type}(" . Perlito5::Java::to_native_args($self->{arguments}) . ")";
                }
                if ($self->{_no_params}) {
                    # Sample->NAME
                    return "$info->{java_type}.${meth}";
                }
                else {
                    return "$info->{java_type}.${meth}(" . Perlito5::Java::to_native_args($self->{arguments}) . ")";
                }
            }
        }

        my $invocant = $self->{invocant}->emit_java($level, 'scalar');

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
                if ($self->{_no_params}) {
                    # $s->NAME
                    return "$invocant.${meth}";
                }
                else {
                    return "$invocant.${meth}(" . Perlito5::Java::to_native_args($self->{arguments}) . ")";
                }
            }
        }

        # method call on a "native" invocant
        #   package Sample { import => "misc.Java.Sample" };
        #   my Sample $s;  
        #   $s->meth()->meth();
        #
        if ( Perlito5::Java::is_native($self->{invocant}) ) {
            if ($self->{_no_params}) {
                # $s->NAME
                return "$invocant.${meth}";
            }
            else {
                return "$invocant.${meth}(" . Perlito5::Java::to_native_args($self->{arguments}) . ")";
            }
        }

        # type coercion method call on an untyped invocant
        #   package Sample { import => "misc.Java.Sample" };
        #   my $x;  
        #   $x->to_Sample();
        #
        if ( $meth =~ /^to/ ) {
            # TODO - check for no-arguments
            my $Java_class = Perlito5::Java::get_java_class_info();
            for my $info ( values %{$Java_class} ) {
                if ( $meth eq $info->{perl_to_java} ) {
                    return "$invocant.$meth()";
                }
            }
        }

        # "Perl" method call

        if ( ref($meth) eq 'Perlito5::AST::Var' ) {
            $meth = $meth->emit_java($level, 'scalar');
        }
        else {
            $meth = Perlito5::Java::escape_string($meth);
        }
        return 'PerlOp.call(' . $invocant . ', ' 
                         . $meth . ', ' 
                         . Perlito5::Java::to_list($self->{arguments}) . ', '
                         . Perlito5::Java::to_context($wantarray)
                  . ')'
    }

    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'array' )
                    . '.aset(' 
                        . Perlito5::Java::to_num($self->{arguments}, $level+1) . ', ' 
                        . Perlito5::Java::to_scalar([$arguments], $level+1)
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'hash' )
                    . '.hset(' 
                        . Perlito5::Java::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . Perlito5::Java::to_scalar([$arguments], $level+1)
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->{method} eq 'postcircumfix:<[ ]>' ) {
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'array' )
                    . '.aset(' 
                        . Perlito5::Java::to_num($self->{arguments}, $level+1) . ', ' 
                        . $list  . '.shift()'
                    . ')';
        }
        if ( $self->{method} eq 'postcircumfix:<{ }>' ) {
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'hash' )
                    . '.hset(' 
                        . Perlito5::Java::autoquote($self->{arguments}, $level+1, 'list') . ', '
                        . $list  . '.shift()'
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Apply;
{
    sub emit_qr_java {
        my ($regex, $modifier, $level) = @_;
        if ( $modifier->{buf} eq '' && ref( $regex ) eq "Perlito5::AST::Var" && $regex->{sigil} eq '$' ) {
            # return as-is because var may contain a qr//
            return $regex->emit_java($level);
        }
        my %flags = map { $_ => 1 } split //, $modifier->{buf};
        # warn Data::Dumper::Dumper(\%flags);
        my $flag_string = join( " | ", 
            ( $flags{'i'} ? 'Pattern.CASE_INSENSITIVE' : () ),
            ( $flags{'x'} ? 'Pattern.COMMENTS'         : () ),
            ( $flags{'m'} ? 'Pattern.MULTILINE'        : () ),
            ( $flags{'s'} ? 'Pattern.DOTALL'           : () ),
        ) || '0';

        my $s = 'new PlRegex(' . Perlito5::Java::to_str( $regex ) . ', ' . $flag_string . ')';
        if ( ref( $regex ) eq "Perlito5::AST::Buf" ) {
            # precompile regex
            my $label = Perlito5::Java::get_label();
            push @Perlito5::Java::Java_constants, "public static final PlRegex $label = $s;";
            return 'PlCx.' . $label;
        }
        return $s;
    }

    sub emit_regex_java {
        my $op = shift;
        my $var = shift;
        my $regex = shift;
        my $level     = shift;
        my $wantarray = shift;

        if ($regex->isa('Perlito5::AST::Var')) {
            # $x =~ $regex
            $regex = { code => 'p5:m', arguments => [ $regex, '' ] };
        }

        my $str;
        my $code = $regex->{code};
        my $regex_args = $regex->{arguments};
        if ($code eq 'p5:s') {
            my $replace = $regex_args->[1];
            my $modifier = $regex_args->[2]->{buf};
            if (ref($replace) eq 'Perlito5::AST::Block') {
                $replace = Perlito5::AST::Apply->new(
                            code => 'do',
                            arguments => [$replace]
                        );
                $modifier =~ s/e//g;
            }
            $str = 'PerlOp.replace('
                    . $var->emit_java() . ', '
                    . emit_qr_java( $regex_args->[0], $modifier, $level ) . ', '
                    . $replace->emit_java() . ', '
                    . Perlito5::Java::to_context($wantarray)
                  . ")";
        }
        elsif ($code eq 'p5:m') {
            $str = 'PerlOp.match('
                    . $var->emit_java() . ', '
                    . emit_qr_java( $regex_args->[0], $regex_args->[1], $level ) . ', '
                    . Perlito5::Java::to_context($wantarray)
                  . ")";
        }
        elsif ($code eq 'p5:tr') {
            $str = "PerlOp.tr("
                    . $var->emit_java() . ', '
                    . $regex_args->[0]->emit_java() . ', '
                    . $regex_args->[1]->emit_java() . ', '
                    . Perlito5::Java::escape_string($regex_args->[2]->{buf}) . ', '
                    . Perlito5::Java::to_context($wantarray)
                  . ")",
        }
        else {
            die "Error: regex emitter - unknown operator $code";
        }

        if ($op eq '=~') {
            return $str;
        }
        if ($op eq '!~') {
            return '!(' . $str . ')'
        }
        die "Error: regex emitter";
    }

    sub emit_java_set {
        my ($self, $arguments, $level, $wantarray) = @_;
        my $code = $self->{code};
        if ($code eq 'prefix:<$>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . '.scalar_deref_set('
                . Perlito5::Java::to_scalar([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<@>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'array' ) . '.array_deref_set('
                . Perlito5::Java::to_list([$arguments], $level+1)  
                . ')';
        }
        if ($code eq 'prefix:<%>') {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'hash' ) . '.hash_deref_set('
                . Perlito5::Java::to_list([$arguments], $level+1)  
                . ')';
        }

        if ($code eq 'prefix:<*>') {
            return 'PlCORE.die("prefix:<*> not implemented")';
            return 'p5typeglob_deref_set(' 
                . Perlito5::Java::to_scalar($self->{arguments}, $level+1) . ', '
                . Perlito5::Java::to_scalar([$arguments], $level+1)       . ', '
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        my $open  = $wantarray eq 'void' ? '' : '(';
        my $close = $wantarray eq 'void' ? '' : ')';
        $open . $self->emit_java( $level+1 ) . ' = ' . $arguments->emit_java( $level+1 ) . $close;
    }

    my %emit_js = (
        'infix:<=~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'infix:<!~>' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '!~', $self->{arguments}->[0], $self->{arguments}->[1], $level, $wantarray );
        },
        'p5:s' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:m' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[2], $self, $level, $wantarray );
        },
        'p5:tr' => sub {
            my ($self, $level, $wantarray) = @_;
            emit_regex_java( '=~', $self->{arguments}->[3], $self, $level, $wantarray );
        },
        'p5:qr' => sub {
            my ($self, $level, $wantarray) = @_;
            return emit_qr_java($self->{arguments}[0], $self->{arguments}[1], $level);
        },
        '__PACKAGE__' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Java::escape_string($Perlito5::PKG_NAME);
        },
        '__SUB__' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::AST::Sub::SUB_REF // 'this'
        },
        'wantarray' => sub {
            my ($self, $level, $wantarray) = @_;
            '(want == PlCx.VOID ? PlCx.UNDEF : new PlInt(want-1))';
        },
        'package' => sub {
            '';
        },
        'uc' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlString('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().toUpperCase())'
        },
        'lc' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlString('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().toLowerCase())'
        },
        'ucfirst' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.ucfirst()'
        },
        'lcfirst' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.lcfirst()'
        },
        'quotemeta' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.quotemeta()'
        },
        'index' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().indexOf('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.toString()))'
        },
        'rindex' => sub {
            my ($self, $level, $wantarray) = @_;
            if($self->{arguments}->[2]) {
                'new PlInt('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().lastIndexOf('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.toString(), ' . $self->{arguments}->[2]->emit_java($level, 'scalar') . '.to_int()))'
            }
            else {
                'new PlInt('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.toString().lastIndexOf('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.toString()))'
            }
        },
        'ord' => sub {
            my ($self, $level, $wantarray) = @_;
            'PerlOp.ord(' . Perlito5::Java::to_str($self->{arguments}->[0], $level) . ')'
        },
        'chr' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlString((char)'
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long())'
        },
        'int' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long())'
        },
        'rand' => sub {
            my ($self, $level, $wantarray) = @_;
              'PerlOp.rand('
            . ( $self->{arguments}->[0]
              ? $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_double()'
              : '1.0'
              )
            . ')'
        },
        'srand' => sub {
            my ($self, $level, $wantarray) = @_;
              'PerlOp.srand('
            . ( $self->{arguments}->[0]
              ? $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long()'
              : ''
              )
            . ')'
        },
        ( map {
                my $op = $_;
                ( $op => sub {
                        my ($self, $level, $wantarray) = @_;
                        $self->{arguments}->[0]->emit_java($level, 'scalar') . '.' . $op . '()'
                      }
                )
            }
            qw/ abs sqrt cos sin exp log /
        ),
        'infix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() % '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<>>>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() >>> '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<<<>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() << '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<^>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() ^ '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() & '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<|>' => sub {
            my ($self, $level, $wantarray) = @_;
              'new PlInt('
            . $self->{arguments}->[0]->emit_java($level, 'scalar') . '.to_long() | '
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . '.to_long())'
        },
        'infix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.add('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.sub('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<*>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.mul('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:</>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.div('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<==>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_eq('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<!=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_ne('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<>>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_gt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<>=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_ge('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_lt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<=>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_le('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<eq>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_eq('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<ne>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_ne('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },

        'infix:<gt>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_gt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<ge>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_ge('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<lt>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_lt('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<le>' => sub {
            my ($self, $level, $wantarray) = @_;
              $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_le('
            . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<~~>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg0 = $self->{arguments}->[0];
            my $arg1 = $self->{arguments}->[1];
            # TODO - test argument type
            #   See: http://perldoc.perl.org/perlop.html#Smartmatch-Operator
            # if (Perlito5::Java::is_num($arg1)) {
            #     # ==
            # }
            'PerlOp.smartmatch_scalar('
                . $arg0->emit_java($level, 'scalar') . ', '
                . $arg1->emit_java($level, 'scalar') . ')'
        },

        'infix:<&&>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  Perlito5::Java::to_bool($self->{arguments}->[0], $level) . ' ? '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ' : PlCx.UNDEF';
            }
            # and1(x) ? y : and3()
            'PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ' : PerlOp.and3()'
        },
        'infix:<and>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  Perlito5::Java::to_bool($self->{arguments}->[0], $level) . ' ? '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ' : PlCx.UNDEF';
            }
            # and1(x) ? y : and3()
            'PerlOp.and1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ' : PerlOp.and3()'
        },
        'infix:<||>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  Perlito5::Java::to_bool($self->{arguments}->[0], $level) . ' ? '
                . ' PlCx.UNDEF : ' . $self->{arguments}->[1]->emit_java($level, 'scalar');
            }
            # or1(x) ? or2() : y
            'PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ''
        },
        'infix:<or>' => sub {
            my ($self, $level, $wantarray) = @_;
            if ($wantarray eq 'void') {
                return
                  Perlito5::Java::to_bool($self->{arguments}->[0], $level) . ' ? '
                . ' PlCx.UNDEF : ' . $self->{arguments}->[1]->emit_java($level, 'scalar');
            }
            # or1(x) ? or2() : y
            'PerlOp.or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.or2() : '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ''
        },
        'infix:<xor>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::Java::to_bool( $self->{arguments}->[0], $level ) . ' ? new PlBool(!'
                 . Perlito5::Java::to_bool($self->{arguments}->[1], $level) . ') : '
                 . ( $self->{arguments}->[1] )->emit_java( $level, $wantarray ) . ')';
        },
        'infix:<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
              Perlito5::AST::Lookup->autoquote($self->{arguments}[0])->emit_java($level)  . ', ' 
            . $self->{arguments}[1]->emit_java($level)
        },
        'infix:<cmp>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.str_cmp('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<<=>>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.num_cmp('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'infix:<**>' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.pow('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'atan2' => sub {
            my ($self, $level, $wantarray) = @_;
                  $self->{arguments}->[0]->emit_java($level, 'scalar') . '.atan2('
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ')'
        },
        'prefix:<!>' => sub {
            my $self      = shift;
            my $level     = shift;
            'new PlBool(!(' . Perlito5::Java::to_bool( $self->{arguments}->[0], $level ) . '))';
        },
        'prefix:<not>' => sub {
            my $self      = shift;
            my $level     = shift;
            my $arg = pop(@{$self->{arguments}});
            if (!$arg) {
                return 'PlCx.TRUE';
            }
            'new PlBool(!( ' . Perlito5::Java::to_bool( $arg, $level ) . '))';
        },
        'prefix:<~>' => sub {
            my $self = $_[0];
            'new PlInt(~' . Perlito5::Java::to_num( $self->{arguments}->[0] ) . '.to_long())';
        },
        'prefix:<->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa('Perlito5::AST::Int')) {
                $arg->{int} = -$arg->{int};
                return $arg->emit_java( $level, 'scalar' );
            }
            if ($arg->isa('Perlito5::AST::Num')) {
                $arg->{num} = -$arg->{num};
                return $arg->emit_java( $level, 'scalar' );
            }
            $arg->emit_java( $level, 'scalar' ) . '.neg()';
        },
        'prefix:<+>' => sub {
            my ($self, $level, $wantarray) = @_;
            $self->{arguments}->[0]->emit_java( $level, $wantarray );
        },
        'require' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            if ($arg->{is_version_string}) {
                # require VERSION
                return 'p5pkg["Perlito5"]["test_perl_version"]([' 
                        . Perlito5::Java::to_str( $self->{arguments}[0] )
                    . '], ' . Perlito5::Java::to_context($wantarray) . ')';
            }
            # require FILE
            'p5pkg["Perlito5::Grammar::Use"]["require"]([' 
                . Perlito5::Java::to_str( $self->{arguments}[0] ) . ', ' 
                . ($self->{arguments}[0]{bareword} ? 1 : 0) 
            . '], ' . Perlito5::Java::to_context($wantarray) . ')';
        },
        'select' => sub {
            my ($self, $level, $wantarray) = @_;
            'p5pkg["CORE"]["select"]([' 
                . ( $self->{arguments}[0]{bareword}
                  ? Perlito5::Java::to_str( $self->{arguments}[0] )
                  : $self->{arguments}[0]->emit_java( $level, 'scalar' ) )
            . '])';
        },
        'prefix:<$>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg  = $self->{arguments}->[0];
            return $arg->emit_java( $level ) . '.scalar_deref()'
        },
        'prefix:<@>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            my $s = Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref()';
            return $wantarray eq 'scalar'
                ? "$s.scalar()"
                : $s;
        },
        'prefix:<$#>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return  Perlito5::Java::emit_java_autovivify( $arg, $level, 'array' ) . '.array_deref().end_of_array_index()';
        },
        'prefix:<%>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            return Perlito5::Java::emit_java_autovivify( $arg, $level, 'hash' ) . '.hash_deref()';
        },
        'prefix:<&>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            'p5code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->emit_java($level) . ')([])';
        },
        'circumfix:<[ ]>' => sub {
            my ($self, $level, $wantarray) = @_;
            'new PlArrayRef(' . Perlito5::Java::to_list( $self->{arguments} ) . ')';
        },
        'circumfix:<{ }>' => sub {
            my ($self, $level, $wantarray) = @_;
            '(new PlHashRef(new PlHash(' . Perlito5::Java::to_list( $self->{arguments}, $level ) . ')))';
        },
        'prefix:<\\>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( $arg->isa('Perlito5::AST::Apply') ) {
                if ( $arg->{code} eq 'prefix:<@>' ) {
                    return 'new PlArrayRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->{code} eq 'prefix:<%>' ) {
                    return 'new PlHashRef(' . $arg->emit_java($level) . ')';
                }
                # if ( $arg->{code} eq '*' ) {
                #     # TODO
                #     return '(new PlGlobRef(' . $arg->emit_java($level) . '))';
                # }
                if ( $arg->{code} eq 'circumfix:<( )>' ) {
                    # \( @x )
                    return 'p5_list_of_refs(' . Perlito5::Java::to_list( $arg->{arguments} ) . ')';
                }
                if ( $arg->{code} eq 'prefix:<&>' ) {
                    return 'p5code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg->{arguments}->[0]->emit_java($level) . ')';
                }
            }
            if ( $arg->isa('Perlito5::AST::Var') ) {
                if ( $arg->sigil eq '@' ) {
                    return 'new PlArrayRef(' . $arg->emit_java($level) . ')';
                }
                if ( $arg->sigil eq '%' ) {
                    return '(new PlHashRef(' . $arg->emit_java($level) . '))';
                }
                if ( $arg->sigil eq '*' ) {
                    return '(new PlGlobRef(' . $arg->emit_java($level) . '))';
                }
                if ( $arg->sigil eq '&' ) {
                    if ( $arg->{namespace} ) {
                        return 'p5pkg[' . Perlito5::Java::escape_string($arg->{namespace} ) . '].' . $arg->{name};
                    }
                    else {
                        return Perlito5::Java::pkg() . '.' . $arg->{name};
                    }
                }
            }
            return '(new PlLvalueRef(' . $arg->emit_java($level) . '))';
        },
        'postfix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return Perlito5::Java::to_native_num($arg, $level, $wantarray) . '++';
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.post_incr()'
        },
        'postfix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return Perlito5::Java::to_native_num($arg, $level, $wantarray) . '--';
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.post_decr()'
        },
        'prefix:<++>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return '++' . Perlito5::Java::to_native_num($arg, $level, $wantarray);
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.pre_incr()'
        },
        'prefix:<-->' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if ( ref($arg) eq 'Perlito5::AST::Var' && $arg->{_id} ) {
                my $id = $arg->{_id};
                my $Java_var = Perlito5::Java::get_java_var_info();
                my $type = $Java_var->{ $id }{type} || 'PlLvalue';
                if ($type ne 'PlLvalue') {
                    return '--' . Perlito5::Java::to_native_num($arg, $level, $wantarray);
                }
            }
            $arg->emit_java($level, 'scalar', 'lvalue') . '.pre_decr()'
        },

        'infix:<x>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg   = $self->{arguments}->[0];
            if (  ref($arg) eq 'Perlito5::AST::Apply'
               && ( $arg->{code} eq 'circumfix:<( )>' || $arg->{code} eq 'list:<,>' )
               )
            {
                # ($v) x $i
                # qw( 1 2 3 ) x $i
                return 'PerlOp.list_replicate('
                           . Perlito5::Java::to_list( [$self->{arguments}->[0] ], $level) . ', '
                           . Perlito5::Java::to_num($self->{arguments}->[1], $level) . ', '
                           . Perlito5::Java::to_context($wantarray)
                        . ')'
            }
            'PerlOp.string_replicate('
                           . Perlito5::Java::to_str($self->{arguments}->[0], $level) . ','
                           . Perlito5::Java::to_num($self->{arguments}->[1], $level) . ')'
        },

        'list:<.>' => sub {
            my ($self, $level, $wantarray) = @_;
            'new PlString(' . join( ' + ', map( Perlito5::Java::to_native_str($_, $level, 'scalar'), @{ $self->{arguments} } ) ) . ')';
        },
        'list:<,>' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Java::to_list( $self->{arguments} );
        },
        'infix:<..>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'PerlOp.range(' . $self->{arguments}->[0]->emit_java($level) . ', '
                              . $self->{arguments}->[1]->emit_java($level) . ', '
                              . Perlito5::Java::to_context($wantarray) . ', '
                              . '"' . Perlito5::Java::get_label() . '"' . ', '
                              . '0'
                        . ')'
        },
        'infix:<...>' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'PerlOp.range(' . $self->{arguments}->[0]->emit_java($level) . ', '
                              . $self->{arguments}->[1]->emit_java($level) . ', '
                              . Perlito5::Java::to_context($wantarray) . ', '
                              . '"' . Perlito5::Java::get_label() . '"' . ', '
                              . '1'
                        . ')'
        },
        'delete' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && ($v->sigil eq '$' || $v->sigil eq '@')
                   )
                {
                    return $v->emit_java($level, $wantarray) . '.delete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'hash') . '.delete(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && ($v->sigil eq '$' || $v->sigil eq '@')
                   )
                {
                    return $v->emit_java($level, $wantarray) . '.delete('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . $arg->{index_exp}->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'array') . '.delete(' . $arg->{index_exp}->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.delete(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_java($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.delete(' . $arg->{arguments}->emit_java($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                die 'TODO delete &code';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                die 'TODO delete &$code';
            }
        },

        'scalar' => sub {
            my ($self, $level, $wantarray) = @_;
            Perlito5::Java::to_scalar($self->{arguments}, $level+1);
        },

        'ternary:<? :>' => sub {
            my ($self, $level, $wantarray) = @_;
            '( ' . Perlito5::Java::to_bool( $self->{arguments}->[0], $level ) . ' ? ' . ( $self->{arguments}->[1] )->emit_java( $level, $wantarray ) . ' : ' . ( $self->{arguments}->[2] )->emit_java( $level, $wantarray ) . ')';
        },
        'my' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of my($x,$y)
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'our' => sub {
            my ($self, $level, $wantarray) = @_;
            # this is a side-effect of our($x,$y)
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'local' => sub {
            my ($self, $level, $wantarray) = @_;
            # 'local ($x, $y[10])'
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'circumfix:<( )>' => sub {
            my ($self, $level, $wantarray) = @_;
            'PerlOp.context(' . join( ', ', Perlito5::Java::to_context($wantarray), map( $_->emit_java( $level, $wantarray ), @{ $self->{arguments} } ) ) . ')';
        },
        'infix:<=>' => sub {
            my ($self, $level, $wantarray) = @_;
            my $parameters = $self->{arguments}->[0];
            my $arguments  = $self->{arguments}->[1];

            if (   $parameters->isa( 'Perlito5::AST::Apply' )
               &&  ( $parameters->code eq 'my' || $parameters->code eq 'local' || $parameters->code eq 'circumfix:<( )>' )
               )
            {
                # my ($x, $y) = ...
                # local ($x, $y) = ...
                # ($x, $y) = ...

                if ( $wantarray eq 'void' ) {
                    my $tmp  = Perlito5::Java::get_label();
                    return join( ";\n" . Perlito5::Java::tab($level),
                            'PlArray ' . $tmp  . ' = ' . Perlito5::Java::to_list([$arguments], $level+1),
                            ( map $_->emit_java_set_list($level, $tmp),
                                  @{ $parameters->arguments }
                            ),
                    );
                }

                my $tmp  = Perlito5::Java::get_label();
                my $tmp2 = Perlito5::Java::get_label();
                return Perlito5::Java::emit_wrap_java($level, 
                            'PlArray ' . $tmp  . ' = ' . Perlito5::Java::to_list([$arguments], $level+1) . ";",
                            'PlArray ' . $tmp2 . ' = ' . $tmp . ".slice(0);",
                            ( map $_->emit_java_set_list($level+1, $tmp) . ";",
                                  @{ $parameters->arguments }
                            ),
                            'return ' . $tmp2,
                );
            }
            return $parameters->emit_java_set($arguments, $level+1, $wantarray);
        },

        'break' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            die "TODO - break() not implemented";
        },
        'next' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            if ($label == 0) {
                return 'PerlOp.next()';
            }
            'PerlOp.next(' . $label . ')';
        },
        'last' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            if ($label == 0) {
                return 'PerlOp.last()';
            }
            'PerlOp.last(' . $label . ')';
        },
        'redo' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            my $label = Perlito5::Java::get_java_loop_label( $self->{arguments}[0]{code} );
            'PerlOp.redo(' . $label . ')';
        },
        'return' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW_RETURN = 1;
            if ( ! @{ $self->{arguments} } ) {
                return 'PerlOp.ret(PerlOp.context(want))';
            }
            'PerlOp.ret(' . Perlito5::Java::to_runtime_context( $self->{arguments}, $level+1 ) . ')';
        },
        'goto' => sub {
            my ($self, $level, $wantarray) = @_;
            $Perlito5::THROW = 1;
            die "TODO - goto() not implemented";
        },
        'caller' => sub {
            my ($self, $level, $wantarray) = @_;
            return 'PerlOp.caller('
                            . Perlito5::Java::to_context($wantarray) . ', '
                            . $self->{arguments}->[0]->emit_java($level)
                        . ')'
        },

        'do' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # do BLOCK
                # rewrite to:   sub {...}->()
                my $ast = Perlito5::AST::Call->new(
                    'method' => 'postcircumfix:<( )>',
                    'invocant' => Perlito5::AST::Sub->new(
                        'block' => $arg,
                        'attributes' => [],
                        _do_block => 1,
                    ),
                    'arguments' => [],
                );
                return $ast->emit_java( $level + 1, $wantarray );
            }

            # do EXPR
            my $tmp_strict = $Perlito5::STRICT;
            $Perlito5::STRICT = 0;
            my $ast =
                Perlito5::AST::Apply->new(
                    code => 'eval',
                    namespace => '',
                    arguments => [
                       Perlito5::AST::Apply->new(
                          code => 'do_file',
                          namespace => 'Perlito5::Grammar::Use',
                          arguments => $self->{arguments}
                        )
                    ],
                    _scope => Perlito5::Grammar::Scope->new_base_scope(),
                );
            my $js = $ast->emit_java( $level, $wantarray );
            $Perlito5::STRICT = $tmp_strict;
            return $js;
        },

        'eval' => sub {
            my ($self, $level, $wantarray) = @_;

            my $arg = $self->{arguments}->[0];
            my $eval;
            if ($arg->isa( "Perlito5::AST::Block" )) {
                # eval BLOCK
                # rewrite to:   sub {...}->()
                my $ast = Perlito5::AST::Call->new(
                    'method' => 'postcircumfix:<( )>',
                    'invocant' => Perlito5::AST::Sub->new(
                        'block' => $arg,
                        'attributes' => [],
                        _eval_block => 1,
                    ),
                    'arguments' => [],
                );
                return $ast->emit_java( $level + 1, $wantarray );
            }
            else {
                # eval string
                die "Java eval string not yet implemented";
            }

            # TODO - test return() from inside eval

            my $context = Perlito5::Java::to_context($wantarray);

            Perlito5::Java::emit_wrap_java($level,
                ( $context eq 'p5want'
                  ? ()
                  : "var want = " . $context . ";",
                ),
                "var r;",
                'p5pkg["main"]["v_@"] = "";',
                'var p5strict = p5pkg["Perlito5"]["v_STRICT"];',
                'p5pkg["Perlito5"]["v_STRICT"] = ' . $Perlito5::STRICT . ';',
                "try {",
                    [ 'r = ' . $eval . "",
                    ],
                "}",
                "catch(err) {",
                [  "if ( err instanceof p5_error || err instanceof Error ) {",
                     [ 'p5pkg["main"]["v_@"] = err;',
                       'if (p5str(p5pkg["main"]["v_@"]).substr(-1, 1) != "\n") {',
                           [ # try to add a stack trace
                             'try {' . "",
                                 [ 'p5pkg["main"]["v_@"] = p5pkg["main"]["v_@"] + "\n" + err.stack + "\n";',
                                 ],
                             '}',
                             'catch(err) { }',
                           ],
                       '}',
                     ],
                   "}",
                   "else {",
                     [ "return(err);",
                     ],
                   "}",
                 ],
                "}",
                'p5pkg["Perlito5"]["v_STRICT"] = p5strict;',
                "return r;",
            );
        },

        'length' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = shift @{$self->{arguments}};
                return Perlito5::Java::to_str($arg) 
                    . '.length('
                    . ')'
        },
        'substr' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = shift @{$self->{arguments}};
                return Perlito5::Java::to_str($arg) 
                    . '.substr('
                    .   join(', ', map( $_->emit_java($level, 'scalar'), @{$self->{arguments}} ))
                    . ')'
        },
        'undef' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                my $arg = $self->{arguments}[0];
                if (  ref( $arg ) eq 'Perlito5::AST::Var' 
                   && $arg->{sigil} eq '&'
                   )
                {
                    return '(delete p5pkg[' . Perlito5::Java::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::Java::escape_string($arg->{name} ) . '])';
                }
                $self->{arguments} = [];
                return $arg->emit_java_set($self, $level, $wantarray);
            }
            return 'PlCx.UNDEF'
        },
        'defined' => sub { 
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}[0];
            my $invocant;
            if (  ref( $arg ) eq 'Perlito5::AST::Apply' 
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2   = $arg->{arguments}->[0];
                $invocant = 'p5code_lookup_by_name(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME ) . ', ' . $arg2->emit_java($level) . ')';
            }
            elsif (  ref( $arg ) eq 'Perlito5::AST::Var' 
               && $arg->{sigil} eq '&'
               )
            {
                $invocant = 'p5pkg[' . Perlito5::Java::escape_string(($arg->{namespace} || $Perlito5::PKG_NAME) ) . '][' . Perlito5::Java::escape_string($arg->{name} ) . ']';
            }
            else {
                $invocant = $arg->emit_java($level, 'scalar');
            }
            # TODO - use this code for typed variables:
            #   'new PlBool(' . $invocant . ' != null)' 
            'new PlBool(!' . $invocant . '.is_undef())'
        },
        'shift' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_java( $level ) . '.shift()'
            }
            return 'List__.shift()'
        },
        'pop' => sub {
            my ($self, $level, $wantarray) = @_;
            if ( $self->{arguments} && @{$self->{arguments}} ) {
                return $self->{arguments}[0]->emit_java( $level ) . '.pop()'
            }
            return 'List__.pop()'
        },
        'unshift' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'
            return $v->emit_java( $level ) . '.unshift(' . Perlito5::Java::to_list(\@arguments) . ')';
        },
        'push' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'
            return $v->emit_java( $level ) . '.push(' . Perlito5::Java::to_list(\@arguments) . ')';
        },
        'time' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.time(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'sleep' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.sleep(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'ref' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.ref(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'exit' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.exit(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'warn' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.warn(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'die' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.die(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'system' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.system(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'qx' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.qx(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'tie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            my $meth;
            if ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '%' ) {
                $meth = 'hash';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '@' ) {
                $meth = 'array';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '$' ) {
                $meth = 'scalar';
            }
            else {
                die "tie '", ref($v), "' not implemented";
            }
            return 'p5tie_' . $meth . '(' . $v->emit_java( $level ) . ', ' . Perlito5::Java::to_list(\@arguments) . ')';
        },
        'untie' => sub {
            my ($self, $level, $wantarray) = @_;
            my @arguments = @{$self->{arguments}};
            my $v = shift @arguments;     # TODO - this argument can also be a 'Decl' instead of 'Var'

            my $meth;
            if ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '%' ) {
                $meth = 'hash';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '@' ) {
                $meth = 'array';
            }
            elsif ( $v->isa('Perlito5::AST::Var') && $v->sigil eq '$' ) {
                $meth = 'scalar';
            }
            else {
                die "tie '", ref($v), "' not implemented";
            }
            return 'p5untie_' . $meth . '(' . $v->emit_java( $level ) . ')';
        },
        'print' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_java( $level );
            }
            else {
                $fun  = 'PlCx.STDOUT';
            }
            my $list = Perlito5::Java::to_list(\@in);
            'PlCORE.print(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'say' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_java( $level );
            }
            else {
                $fun  = 'PlCx.STDOUT';
            }
            my $list = Perlito5::Java::to_list(\@in);
            'PlCORE.say(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'printf' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            if ( $self->{special_arg} ) {
                $fun  = $self->{special_arg}->emit_java( $level );
            }
            else {
                $fun  = 'PlCx.STDOUT';
            }
            my $list = 'new PlArray(PlCORE.sprintf(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list(\@in) . '))';
            'PlCORE.print(' . Perlito5::Java::to_context($wantarray) . ', ' . $fun . ', ' . $list . ')';
        },
        'hex' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.hex(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'oct' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.oct(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'sprintf' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.sprintf(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'crypt' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.crypt(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'join' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.join(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'reverse' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.reverse(' . Perlito5::Java::to_context($wantarray) . ', ' . Perlito5::Java::to_list($self->{arguments}) . ')';
        },
        'values' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.values(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'keys' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.keys(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'each' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.each(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'close' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            'p5pkg["Perlito5::IO"].close(' . $fun->emit_java( $level ) . ', [])';
        },
        'open' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            if (ref($fun) ne 'Perlito5::AST::Apply') {
                # doesn't look like STDERR or FILE; initialize the variable with a GLOB
                return Perlito5::Java::emit_wrap_java($level,
                    $fun->emit_java( $level ) . ' = CORE.bless([ {file_handle : {id : null}}, "GLOB" ]);',
                    'return CORE.open(' . Perlito5::Java::to_list( $self->{arguments}, $level ) . ')'
                );
            }
            else {
                $Perlito5::STRICT = 0;  # allow FILE bareword
                return 'CORE.open(' . Perlito5::Java::to_list( $self->{arguments}, $level ) . ')'
            }
        },
        'chomp' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.chomp(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'chop' => sub {
            my ($self, $level, $wantarray) = @_;
            'PlCORE.chop(' . Perlito5::Java::to_context($wantarray) . ', ' . $self->{arguments}[0]->emit_java($level) . ')';
        },
        'read' => sub {
            my ($self, $level, $wantarray) = @_;
            # read FILEHANDLE,SCALAR,LENGTH,OFFSET
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in);
            my $scalar = shift(@in);
            my $length = shift(@in);
            return Perlito5::Java::emit_wrap_java($level,
                'var r = p5pkg["Perlito5::IO"].read(' . $fun->emit_java( $level ) . ', [' . $length->emit_java( $level ) . ']);',
                $scalar->emit_java( $level ) . ' = r[1];',
                'return r[0]',
            );
        },
        'readline' => sub {
            my ($self, $level, $wantarray) = @_;
            # readline FILEHANDLE
            # TODO - special cases; see 'readline' and '<>' in "perldoc perlop"
            my @in  = @{$self->{arguments}};
            my $fun = shift(@in)
                || bless({
                       'arguments' => [],
                       'bareword' => 1,
                       'code' => 'ARGV',
                       'namespace' => '',
                   }, 'Perlito5::AST::Apply');
            return 'CORE.readline([' . $fun->emit_java( $level ) . '])';
        },
        'map' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Java::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.map(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . Perlito5::Java::to_context($wantarray) . ')';
        },
        'grep' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in = @{$self->{arguments}};

            my $fun;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                $fun  = shift @in;
            }
            my $list = Perlito5::Java::to_list(\@in);

            if (ref($fun) eq 'Perlito5::AST::Block') {
                $fun = $fun->{stmts}
            }
            else {
                $fun = [$fun];
            }

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.grep(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . Perlito5::Java::to_context($wantarray) . ')';
        },
        'bless' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $ref = shift @in;
            my $class = shift @in;

            return $ref->emit_java( $level, "scalar" )
                . '.bless(' . $class->emit_java( $level, "scalar" ) . ')';
        },
        'sort' => sub {
            my ($self, $level, $wantarray) = @_;
            my @in  = @{$self->{arguments}};
            my $fun;
            my $list;

            if ( $self->{special_arg} ) {
                # TODO - test 'special_arg' type (scalar, block, ...)
                $fun  = $self->{special_arg};
            }
            else {
                if (ref($in[0]) eq 'Perlito5::AST::Block') {
                    # the sort function is optional
                    $fun  = shift @in;
                }
            }

            if (ref($fun) eq 'Perlito5::AST::Block') {
                # the sort function is optional
                $fun = $fun->{stmts};
            }
            else {
                die "TODO: sort without block not implemented yet";
            }
            $list = Perlito5::Java::to_list(\@in);

            my $sub = Perlito5::AST::Sub->new( block => Perlito5::AST::Block->new( stmts => $fun ) );

            'PerlOp.sort(' . $sub->emit_java( $level + 1 ) . ', '
                . $list . ', '
                . Perlito5::Java::to_context($wantarray) . ', '
                . Perlito5::Java::pkg . ')';
        },
        'infix:<//>' => sub { 
            my ($self, $level, $wantarray) = @_;
            # defined_or1(x) ? defined_or2() : y
            'PerlOp.defined_or1('
                . $self->{arguments}->[0]->emit_java($level, 'scalar') . ') ? PerlOp.defined_or2() : '
                . $self->{arguments}->[1]->emit_java($level, 'scalar') . ''
        },
        'exists' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            if ($arg->isa( 'Perlito5::AST::Lookup' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    # $v->{sigil} = '%';
                    return $v->emit_java($level, $wantarray) . '.exists(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'hash') . '.exists(' . $arg->autoquote($arg->{index_exp})->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Index' )) {
                my $v = $arg->obj;
                if (  $v->isa('Perlito5::AST::Var')
                   && $v->sigil eq '$'
                   )
                {
                    return $v->emit_java($level, $wantarray) . '.exists(' . $arg->{index_exp}->emit_java($level) . ')';
                }
                return $v->emit_java($level, $wantarray, 'array') . '.exists(' . $arg->{index_exp}->emit_java($level) . ')';
            }
            if ($arg->isa( 'Perlito5::AST::Call' )) {
                if ( $arg->method eq 'postcircumfix:<{ }>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'hash') . '.exists(' . Perlito5::AST::Lookup->autoquote($arg->{arguments})->emit_java($level) . ')';
                }
                if ( $arg->method eq 'postcircumfix:<[ ]>' ) {
                    return $arg->invocant->emit_java($level, $wantarray, 'array') . '.exists(' . $arg->{arguments}->emit_java($level) . ')';
                }
            }
            if (  $arg->isa('Perlito5::AST::Var')
               && $arg->sigil eq '&'
               )
            {
                # TODO exist() + 'my sub'
                my $name = $arg->{name};
                my $namespace = $arg->{namespace} || $Perlito5::PKG_NAME;
                return 'p5pkg[' . Perlito5::Java::escape_string($namespace) . '].hasOwnProperty(' . Perlito5::Java::escape_string($name) . ')';
            }
            if (  $arg->isa('Perlito5::AST::Apply')
               && $arg->{code} eq 'prefix:<&>'
               )
            {
                my $arg2 = $arg->{arguments}->[0];
                return 'p5sub_exists(' . Perlito5::Java::to_str($arg2) . ', ' . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ')';
            }
        },

        'prototype' => sub {
            my ($self, $level, $wantarray) = @_;
            my $arg = $self->{arguments}->[0];
            return 'p5sub_prototype(' . $arg->emit_java() . ', ' . Perlito5::Java::escape_string($Perlito5::PKG_NAME) . ')';
        },
        'split' => sub {
            my ($self, $level, $wantarray) = @_;
            my @js;
            my $arg = $self->{arguments}->[0];
            if ( $arg
              && $arg->isa('Perlito5::AST::Apply')
              && $arg->{code} eq 'p5:m'
            ) {
                # first argument of split() is a regex
                push @js, 'new RegExp('
                        . $arg->{arguments}->[0]->emit_java() . ', '
                        . Perlito5::Java::escape_string($arg->{arguments}->[1]->{buf})
                    . ')';
                shift @{ $self->{arguments} };
            }
            return 'CORE.split('
                . '[' . join( ', ',
                    @js,
                    map( $_->emit_java, @{ $self->{arguments} } ) )
                . '], '
                . Perlito5::Java::to_context($wantarray)
            . ')';
        },
    );

    sub emit_java {
        my ($self, $level, $wantarray) = @_;

        my $apply = $self->op_assign();
        if ($apply) {
            return $apply->emit_java( $level );
        }
        my $apply = $self->op_auto();
        if ($apply) {
            return $apply->emit_java( $level );
        }

        my $code = $self->{code};

        if (ref $code ne '') {
            my @args = ();
            push @args, $_->emit_java
                for @{$self->{arguments}};
            return $self->{code}->emit_java( $level ) . '.apply(' . join(',', @args) . ')';
        }

        return $emit_js{$code}->($self, $level, $wantarray)
            if exists $emit_js{$code};

        if (exists $Perlito5::Java::op_prefix_js_str{$code}) {
            return $Perlito5::Java::op_prefix_js_str{$code} . '(' 
                . Perlito5::Java::to_str($self->{arguments}[0])
                . ')'
        }

        if ($self->{namespace}) {
            if (  $self->{namespace} eq 'Java' 
               && $code eq 'inline'
               ) 
            {
                if ( $self->{arguments}->[0]->isa('Perlito5::AST::Buf') ) {
                    # Java::inline('int x = 123')
                    return $self->{arguments}[0]{buf};
                }
                else {
                    die "Java::inline needs a string constant";
                }
            }
            $code = 'PlV.get(' . Perlito5::Java::escape_string($self->{namespace} . '::' . $code ) . ')'
        }
        else {
            $code = 'PlV.get(' . Perlito5::Java::escape_string($Perlito5::PKG_NAME . '::' . $code ) . ')'
        }

        my $sig;
        my $may_need_autoload;
        {
            my $name = $self->{code};
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            my $effective_name = $namespace . "::" . $self->{code};
            if ( exists $Perlito5::PROTO->{$effective_name} ) {
                $sig = $Perlito5::PROTO->{$effective_name};
            }
            elsif ( (!$self->{namespace} || $namespace eq 'CORE')
                  && exists $Perlito5::CORE_PROTO->{"CORE::$name"}
                  )
            {
                $effective_name = "CORE::$name";
                $sig = $Perlito5::CORE_PROTO->{$effective_name};
            }
            else {
                # this subroutine was never declared
                if ($self->{bareword}) {
                    # TODO: allow barewords where a glob is expected: open FILE, ...
                    if ( $Perlito5::STRICT ) {
                        die 'Bareword ' . Perlito5::Java::escape_string($name ) . ' not allowed while "strict subs" in use';
                    }

                    # bareword doesn't call AUTOLOAD
                    return Perlito5::Java::escape_string( 
                            ($self->{namespace} ? $self->{namespace} . '::' : "") . $name 
                        );
                }
                $may_need_autoload = 1;
            }
            # is there a sig override
            $sig = $self->{proto}
                if (exists $self->{proto});
        }

        if ($sig) {
            # warn "sig $effective_name $sig\n";
            my @out = ();
            my @in  = @{$self->{arguments} || []};

            # TODO - generate the right prototype

            my $optional = 0;
            while (length $sig) {
                my $c = substr($sig, 0, 1);
                if ($c eq ';') {
                    $optional = 1;
                }
                elsif ($c eq '$' || $c eq '_') {
                    push @out, shift(@in)->emit_java( $level + 1, 'scalar' ) if @in || !$optional;
                }
                elsif ($c eq '@') {
                    push @out, Perlito5::Java::to_list(\@in, $level + 1)
                        if @in || !$optional;
                    @in = ();
                }
                elsif ($c eq '&') {
                    push @out, shift(@in)->emit_java( $level + 1, 'scalar' );
                }
                elsif ($c eq '*') {
                    if (@in || !$optional) {
                        my $arg = shift @in;
                        if ($arg->{bareword}) {
                            push @out, Perlito5::Java::escape_string($arg->{code});
                        }
                        else {
                            push @out, $arg->emit_java( $level + 1, 'scalar' );
                        }
                    }
                }
                elsif ($c eq '\\') {
                    if (substr($sig, 0, 2) eq '\\$') {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_java( $level + 1, 'scalar' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 2) eq '\\@'
                        || substr($sig, 0, 2) eq '\\%'
                        )
                    {
                        $sig = substr($sig, 1);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 5) eq '\\[@%]') {
                        $sig = substr($sig, 4);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                    elsif (substr($sig, 0, 6) eq '\\[$@%]') {
                        $sig = substr($sig, 5);
                        push @out, shift(@in)->emit_java( $level + 1, 'list' ) if @in || !$optional;
                    }
                }
                $sig = substr($sig, 1);
            }

            return $code . '.apply('
                        . Perlito5::Java::to_context($wantarray)
                        . ', PlArray.construct_list_of_aliases(' . join(', ', @out) . ')'
                . ')';
        }

        my $items = Perlito5::Java::to_list_preprocess( $self->{arguments} );
        my $arg_code = 'PlArray.construct_list_of_aliases('
             .   join(', ', map( $_->emit_java($level, 'list'), @$items ))
             . ')';

        # TODO - autoload
        # if ( $may_need_autoload ) {
        #     # p5call_sub(namespace, name, list, want)
        #     my $name = $self->{code};
        #     my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
        #     return 'p5call_sub('
        #             . Perlito5::Java::escape_string($namespace) . ', '
        #             . Perlito5::Java::escape_string($name) . ', '
        #             . $arg_code . ', '
        #             . Perlito5::Java::to_context($wantarray)
        #          . ')';
        # }

        $code . '.apply('
                . Perlito5::Java::to_context($wantarray) . ', '
                . $arg_code
              . ')';

    }

    sub emit_java_set_list {
        my ($self, $level, $list) = @_;
        if ( $self->code eq 'undef' ) {
            return $list . '.shift()' 
        }
        if ( $self->code eq 'prefix:<$>' ) {
            return Perlito5::Java::emit_java_autovivify( $self->{arguments}->[0], $level+1, 'scalar' ) . '.scalar_deref_set('
                . $list->emit_java( $level + 1, 'scalar' )
                . ')';
        }
        die "not implemented: assign to ", $self->code;
    }

    sub emit_java_get_decl {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'my' || $code eq 'our' || $code eq 'state' || $code eq 'local') {
            return ( map {     ref($_) eq 'Perlito5::AST::Var'
                             ? Perlito5::AST::Decl->new(
                                 decl => $code,
                                 type => '',     # TODO - add type
                                 var  => $_,
                               )
                             : ()
                         }
                         @{ $self->{arguments} }
                   );
        }
        if ($code ne 'do' && $code ne 'eval') {
            return ( map  +( $_->emit_java_get_decl ), 
                          @{ $self->{arguments} }
                   )
                if $self->{arguments};
        }
        return ()
    }
    sub emit_java_has_regex {
        my $self      = shift;
        my $code = $self->{code};
        if ($code eq 'p5:m' || $code eq 'p5:s' || $code eq 'infix:<=~>' || $code eq 'infix:<!~>') {
            return 1;
        }
        return ()
    }
}

package Perlito5::AST::If;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_java_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_java_init($level, $wantarray);
            }
        }
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? $self->{body} # may be undef
            : (!@{ $self->{body}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, not_a_loop => 1 )
            : Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, create_context => 1, not_a_loop => 1 );
        my $otherwise =
              ref($self->{otherwise}) ne 'Perlito5::AST::Block'
            ? $self->{otherwise}  # may be undef
            : (!@{ $self->{otherwise}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::Java::LexicalBlock->new( block => $self->{otherwise}->stmts, not_a_loop => 1 )
            : Perlito5::Java::LexicalBlock->new( block => $self->{otherwise}->stmts, create_context => 1, not_a_loop => 1 );
 
        push @str, 'if (' . Perlito5::Java::to_bool($cond, $level + 1) . ') {';
        if ($body) {
            push @str, [ $body->emit_java( $level + 1, $wantarray ) ];
        }
        push @str, '}';
        if ($otherwise) {
            if ( @{ $otherwise->{block} } == 1 
               && ref($otherwise->{block}[0]) eq 'Perlito5::AST::If'
               )
            {
                push @str, 'else', [ $otherwise->{block}[0]->emit_java( $level, $wantarray ) ];
            }
            else {
                push @str, 
                    'else {',
                      [ $otherwise->emit_java( $level + 1, $wantarray ) ],
                    '}';
            }
        }
        return Perlito5::Java::emit_wrap_java($level, @str);
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}


package Perlito5::AST::When;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        # TODO - special case when When is inside a Given block
        # TODO - special case when When is a statement modifier
        my $cond = $self->{cond};
        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_java_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_java_init($level, $wantarray);
            }
        }
        $cond = Perlito5::AST::Apply->new(
                'arguments' => [
                    Perlito5::AST::Var->new(
                        'name' => '_',
                        'namespace' => 'main',
                        'sigil' => '$',
                    ),
                    $cond,
                ],
                'code' => 'infix:<~~>',
                'namespace' => '',
            );
        my $next = Perlito5::AST::Apply->new(
                'arguments' => [],
                'bareword' => 1,
                'code' => 'next',
                'namespace' => '',
            );
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? Perlito5::Java::LexicalBlock->new( block => [ $self->{body} ], not_a_loop => 1 )
            : (!@{ $self->{body}->stmts })
            ? undef
            : $wantarray ne 'void'
            ? Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, not_a_loop => 1 )
            : Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, create_context => 1, not_a_loop => 1 );
        push @{ $body->{block} }, $next; 

        push @str, 'if (' . Perlito5::Java::to_bool($cond, $level + 1) . ') {';
        if ($body) {
            push @str, [ $body->emit_java( $level + 1, $wantarray ) ];
        }
        push @str, '}';
        return Perlito5::Java::emit_wrap_java($level, @str);
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}


package Perlito5::AST::While;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $cond = $self->{cond};

        # extract declarations from 'cond'
        my @str;
        my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        if ($cond) {
            my @var_decl = $cond->emit_java_get_decl();
            for my $arg (@var_decl) {
                $level = $old_level + 1;
                push @str, $arg->emit_java_init($level, $wantarray);
            }
        }
        my $expression;
        if (Perlito5::Java::is_native_bool($cond)) {
            $expression = Perlito5::Java::to_native_bool($cond, $level + 1);
        }
        else {
            $expression =  Perlito5::Java::to_bool($cond, $level + 1);    
        }

        if ( ref($self->{body}) eq 'Perlito5::AST::Apply' && $self->{body}{code} eq 'do' ) {
            # body is 'Perlito5::AST::Apply' in this construct:
            #   do { ... } while ...;
            push @str,
                'do {',
                  [ Perlito5::Java::LexicalBlock->new(
                        block => $self->{body}{arguments}[0]{stmts},
                        not_a_loop => 1,
                    )->emit_java($level + 2, $wantarray)
                  ],
                '}',
                'while (' . $expression . ');';
        }
        else {
            local $Perlito5::THROW = 0;
            my $body =
                  ref($self->{body}) ne 'Perlito5::AST::Block'
                ? [ $self->{body} ]
                : $self->{body}{stmts};
            push @str, 'while (' . $expression . ') {',
                          [ Perlito5::Java::LexicalBlock->new(
                                block => $body,
                                block_label => $self->{label},
                                continue => $self->{continue},
                            )->emit_java($level + 2, $wantarray)
                          ],
                        '}';
            if ($Perlito5::THROW) {
                @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str );
            }
        }

        return Perlito5::Java::emit_wrap_java($level, @str);
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::For;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        local $Perlito5::THROW = 0;
        my $body =
              ref($self->{body}) ne 'Perlito5::AST::Block'
            ? [ $self->{body} ]
            : $self->{body}{stmts};

        # extract declarations from 'cond'
        my @str;
        # my $old_level = $level;
        # print Perlito5::Dumper::Dumper($self);
        # print Perlito5::Dumper::Dumper($self->{cond});
        my $cond = ref( $self->{cond} ) eq 'ARRAY'
                   ? $self->{cond}
                   : [ $self->{cond} ];
        for my $expr ( @$cond, $self->{topic} ) {
            if ($expr) {
                my @var_decl = $expr->emit_java_get_decl();
                for my $arg (@var_decl) {
                    # $level = $old_level + 1;
                    push @str, $arg->emit_java_init($level, $wantarray);
                }
            }
        }
        # print Perlito5::Dumper::Dumper(\@str);

        if (ref($self->{cond}) eq 'ARRAY') {
            # C-style for
            # TODO - make continue-block a syntax error
            push @str,
                'for ( '
                    . ( $self->{cond}[0] ? $self->{cond}[0]->emit_java($level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[1] ? Perlito5::Java::to_bool($self->{cond}[1], $level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[2] ? $self->{cond}[2]->emit_java($level + 1) . ' '   : ''  )
                  . ') {',
                      [
                        # $v->emit_java($level + 1) . ".set(item);",
                        (Perlito5::Java::LexicalBlock->new( block => $body, block_label => $self->{label} ))->emit_java($level + 2, $wantarray),
                      ],
                '}';
        }
        else {
            my $cond = Perlito5::Java::to_list([$self->{cond}], $level + 1);
            my $topic = $self->{topic};
            my $local_label = Perlito5::Java::get_label();
            my $decl = '';
            my $v = $topic;
            if ($v->{decl}) {
                $decl = $v->{decl};
                $v    = $v->{var};
            }
            else {
                $decl = $v->{_decl} || 'global';
            }
            my $namespace = $v->{namespace} || $v->{_namespace} || $Perlito5::PKG_NAME;
            my $s;
            if ($decl eq 'my' || $decl eq 'state') {
                push @str,
                        'for (PlObject ' . $local_label . ' : ' . $cond . '.a) {',
                          [ $v->emit_java($level + 1) . ".set($local_label);",
                            Perlito5::Java::LexicalBlock->new(
                                block => $body,
                                block_label => $self->{label},
                                continue => $self->{continue},
                            )->emit_java($level + 2, $wantarray),
                          ],
                        '}';
            }
            else {
                # use global variable or $_
                # TODO - localize variable
                push @str,
                        'for (PlObject ' . $local_label . ' : ' . $cond . '.a) {',
                          [ $v->emit_java($level + 1) . ".set($local_label);",
                            Perlito5::Java::LexicalBlock->new(
                                block => $body,
                                block_label => $self->{label},
                                continue => $self->{continue},
                            )->emit_java($level + 2, $wantarray),
                          ],
                        '}';
            }
        }
        if ($Perlito5::THROW) {
            @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str );
        }
        return Perlito5::Java::emit_wrap_java($level, @str);
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Sub;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $prototype = defined($self->{sig}) 
                        ? 'new PlString(' . Perlito5::Java::escape_string($self->{sig}) . ')'
                        : 'PlCx.UNDEF';

        my $sub_ref = Perlito5::Java::get_label();
        local $Perlito5::AST::Sub::SUB_REF = $sub_ref;
        my $block = Perlito5::Java::LexicalBlock->new( block => $self->{block}{stmts} );

        # get list of captured variables, including inner blocks
        my @captured;
        for my $stmt (@{$self->{block}{stmts}}) {
            push @captured, $stmt->get_captures();
        }

        my %dont_capture = map { $_->{dont} ? ( $_->{dont} => 1 ) : () } @captured;
        my %capture = map { $_->{dont} ? ()
                          : $dont_capture{ $_->{_id} } ? ()
                          : ($_->{_decl} eq 'local' || $_->{_decl} eq 'global' || $_->{_decl} eq '') ? ()
                          : ( $_->{_id} => $_ )
                          } @captured;
        # warn Data::Dumper::Dumper(\@captured);
        # warn Data::Dumper::Dumper(\%dont_capture);
        # warn Data::Dumper::Dumper(\%capture);
        my @captures_ast  = map { $capture{$_} }
                            sort keys %capture;
        my @captures_java = map { $_->emit_java( $level, 'list' ) } @captures_ast;

        # set the new variable names inside the closure
        local %Perlito5::Java::Java_var_name;
        my $i = 0;
        for (@captures_ast) {
            $Perlito5::Java::Java_var_name{ $_->{_id} } = 'this.env[' . $i . ']';
            $i++;
        }

        my @js_block;
        if ($self->{_do_block}) {
            # do-block
            @js_block = $block->emit_java( $level + 3, $wantarray );
        }
        elsif ($self->{_eval_block}) {
            # eval-block
            $block->{top_level} = 1;
            $block->{eval_block} = 1;
            my $outer_throw = $Perlito5::THROW_RETURN;
            $Perlito5::THROW_RETURN = 0;
            @js_block = $block->emit_java( $level + 3, 'runtime' ),
            $Perlito5::THROW_RETURN = $outer_throw;
        }
        else {
            $block->{top_level} = 1;
            my $outer_throw = $Perlito5::THROW_RETURN;
            $Perlito5::THROW_RETURN = 0;
            @js_block = $block->emit_java( $level + 3, 'runtime' );
            $Perlito5::THROW_RETURN = $outer_throw;
        }

        my $s = Perlito5::Java::emit_wrap_java($level,
            "new PlClosure($prototype, new PlObject[]{ " . join(', ', @captures_java) . " } ) {",
                [ "public PlObject apply(int want, PlArray List__) {",
                    \@js_block,
                  "}",
                ],
            "}",
        );

        if ( $self->{name} ) {
            return 'PlV.set(' . Perlito5::Java::escape_string($self->{namespace} . '::' . $self->{name} ) . ", " . $s . ')'
        }
        else {
            return $s;
        }

        # PlClosure c = new PlClosure( "", new PlObject[]{ v1, v2, v3 } ) {
        #     public PerlitoObject apply( context, args ) {
        #         System.out.println("called MyClosure with " + this.env[2].toString());
        #         return new PerlitoInt(0);
        #     }
        # };
        # c.apply( context, args );

    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Use;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        Perlito5::Grammar::Use::emit_time_eval($self);
        if ($wantarray ne 'void') {
            return 'PlCx.UNDEF';
        }
        else {
            return '// ' . $self->{code} . ' ' . $self->{mod} . "\n";
        }
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

1;

=begin

=head1 NAME

Perlito5::Java::Emit - Code generator for Perlito Perl5-in-Java

=head1 SYNOPSIS

    $program->emit_java()  # generated Perl5 code

=head1 DESCRIPTION

This module generates Java code for the Perlito compiler.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.
The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 COPYRIGHT

Copyright 2006, 2009, 2011, 2012 by Flavio Soibelmann Glock, Audrey Tang and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end

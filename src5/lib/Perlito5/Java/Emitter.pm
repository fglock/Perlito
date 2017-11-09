use v5;
use strict;

package Perlito5::Java::LexicalBlock;
{
    sub new { my $class = shift; bless {@_}, $class }
    sub block { $_[0]->{block} }
    # top_level - true if this is the main block in a subroutine;

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

    sub looks_like_dead_code {
        my ($decl) = @_;

        if ( $decl->isa('Perlito5::AST::Apply') && $decl->{code} eq 'circumfix:<( )>' ) {
            # dead code     ()
            return 1 if !@{$decl->{arguments}};
            # dead code     (my $x, undef)
            return 1 if !grep {  !looks_like_dead_code($_)  } @{$decl->{arguments}};
        }
        if ( $decl->isa('Perlito5::AST::Apply') && ($decl->code eq 'my' || $decl->code eq 'our') ) {
            # dead code     my ($x)
            return 1;
        }
        if ( $decl->isa('Perlito5::AST::Decl') && ($decl->decl eq 'my' || $decl->decl eq 'our') ) {
            # dead code     my $x
            return 1;
        }
        if (  ( $decl->isa('Perlito5::AST::Int') )
           || ( $decl->isa('Perlito5::AST::Num') )
           || ( $decl->isa('Perlito5::AST::Buf') )
           || ( $decl->isa('Perlito5::AST::Var') && $decl->{sigil} ne '&' )
           || ( $decl->isa('Perlito5::AST::Apply') && $decl->code eq 'undef' && !@{$decl->{arguments}} )
           )
        {
            # dead code     123
            # dead code     undef
            return 1;
        }
        return 0;
    }

    sub emit_body_statement {
        my ($decl, $level, $wantarray ) = @_;
        my @str;

        if ( ref($decl) eq 'Perlito5::AST::Apply' && $decl->code eq 'package' ) {
            $Perlito5::PKG_NAME = $decl->{namespace};
        }

        my @var_decl = $decl->emit_java_get_decl();
        for my $arg (@var_decl) {
            push @str, $arg->emit_java_init($level, $wantarray);
        }

        if ( looks_like_dead_code( $decl ) ) {
            # this looks like dead code
        }
        elsif ( $decl->isa('Perlito5::AST::Apply')
          && !( $decl->{namespace} eq 'Java' && $decl->{code} eq 'inline' ) 
          && !( $Perlito5::Java::valid_java_statement{ $decl->{code} } ) 
          && !( $decl->{namespace} ne "" && $decl->{namespace} ne "CORE" ) 
          && !( $decl->{code} eq "infix:<&&>" )
          && !( $decl->{code} eq "infix:<||>" )
          && !( $decl->{code} eq "infix:<and>" )
          && !( $decl->{code} eq "infix:<or>" )
          && !( $decl->{code} eq "ternary:<? :>" )
          )
        {
            # workaround for "Error: not a statement"
            push @str, 'PerlOp.statement(' . $decl->emit_java( $level+1, 'void' ) . ');';
        }
        elsif ( $decl->isa('Perlito5::AST::CompUnit')
              || $decl->isa('Perlito5::AST::For' )
              || $decl->isa('Perlito5::AST::While' )
              || $decl->isa('Perlito5::AST::If' )
              || $decl->isa('Perlito5::AST::Block' )
              )
        {
            push @str, $decl->emit_java( $level, 'statement' );
        }
        else {
            push @str, $decl->emit_java( $level, 'statement' ) . ';';
        }
        return @str;
    }

    sub emit_last_statement {
        my ($last_statement, $level, $wantarray, $has_local, $local_label ) = @_;
        my @str;

        my @var_decl = $last_statement->emit_java_get_decl();
        for my $arg (@var_decl) {
            push @str, $arg->emit_java_init($level, $wantarray);
        }

        my @stmt = Perlito5::Macro::insert_return($last_statement);
        $last_statement = pop @stmt;

        for (@stmt) {
            push @str, $_->emit_java($level, 'statement') . ';';
        }

        if ( $last_statement->isa( 'Perlito5::AST::Apply' ) && $last_statement->code eq 'return' ) {
            if ( $self->{top_level} || $last_statement->{_return_from_block} || $Perlito5::JAVA_CAN_RETURN ) {
                if (!@{$last_statement->{arguments}}) {
                    push @str, emit_return($has_local, $local_label, 'PerlOp.context(want)') . ';'; 
                }
                else {
                    push @str, emit_return($has_local, $local_label,
                            Perlito5::Java::to_runtime_context([$last_statement->{arguments}[0]], $level+1, 'runtime')
                        ) . ';';
                }
            }
            else {
                if (!@{$last_statement->{arguments}}) {
                    $Perlito5::THROW_RETURN = 1;
                    push @str, 'return PerlOp.ret(PerlOp.context(return_context));'; 
                }
                else {
                    $Perlito5::THROW_RETURN = 1;
                    push @str, 'return PerlOp.ret('
                        . Perlito5::Java::to_runtime_context([$last_statement->{arguments}[0]], $level+1, 'return')
                        . ');';
                }
            }
        }
        else {
            my $s = $last_statement->emit_java($level, 'runtime');
            $s .= ';'
              unless $last_statement->isa('Perlito5::AST::If')
              || $last_statement->isa('Perlito5::AST::While')
              || $last_statement->isa('Perlito5::AST::Block');
            push @str, $s;
        }
        return @str;
    }

    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        local $Perlito5::PKG_NAME = $Perlito5::PKG_NAME;
        my $block_label = Perlito5::Java::get_java_loop_label( $self->{block_label} );
        $Perlito5::THROW = 1 if $block_label;

        my @block;
        my $block_last = $#{$self->{block}};
      STMT:
        for my $i ( 0 .. $block_last ) {
            my $stmt = $self->{block}[$i];
            if (defined($stmt)) {
                push @block, $stmt;

                # TODO - test for "if" with "return" in both sides

                if ( $stmt->isa( 'Perlito5::AST::Apply' ) && $stmt->code eq 'return' ) {
                    last STMT;
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
            $has_local = 1;
            $has_regex = 1;
        }
        my $local_label = Perlito5::Java::get_label();
        local $Perlito5::JAVA_HAS_LOCAL = $has_local;
        local $Perlito5::JAVA_LOCAL_LABEL = $local_label;
        $Perlito5::JAVA_CAN_RETURN = 0 if $has_local;
        if ( $has_local ) {
            push @pre, 'int ' . $local_label . ' = PerlOp.local_length();';
            if ($has_regex) {
                push @pre, 'PerlOp.push_local_regex_result();'
            }
        }

        my $last_statement;
        if ($wantarray ne 'void' && $wantarray ne 'statement') {
            $last_statement = pop @block;
        }
        for my $decl ( @block ) {
            push @str, emit_body_statement( $decl, $level, 'statement' );
        }
        if ($last_statement) {
            push @str, emit_last_statement( $last_statement, $level, $wantarray, $has_local, $local_label );
        }

        # print STDERR Perlito5::Dumper::Dumper( $self );
        # print STDERR Perlito5::Dumper::Dumper( \@str );

        if ($self->{eval_block}) {
            # eval { ... }
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
                    [ 'PlV.sset("main::@", e.ret);',
                      "return PlCx.UNDEF;",
                    ],
                "}",
                "catch(Exception e) {",
                    [ 'PlV.sset("main::@", new PlStringLazyError(e));',
                      "return PlCx.UNDEF;",
                    ],
                "}",
            );
        }

        if ($self->{top_level}) {
            # sub { ... }
            if ($Perlito5::THROW_RETURN) {
                return ( @pre,
                    "try {",
                       [ @str ],
                    '}',
                    'catch(PlReturnException e) {',
                       [ emit_return($has_local, $local_label, 'e.ret') . ";" ],
                    '}',
                );
            }
            else {
                return ( @pre, @str );
            }
        }

        if ($self->{not_a_loop}) {
            # if (1) { ... } simple lexical block
            if ($has_local && !$last_statement) {
                if (@block && $block[-1]->isa( 'Perlito5::AST::Apply' ) && $block[-1]->code eq 'return' ) {
                }
                else {
                    push @str, 'PerlOp.cleanup_local(' . $local_label . ', PlCx.UNDEF);';
                }
            }
            return ( @pre, @str );
        }

        if ( $Perlito5::THROW || ($self->{continue} && @{$self->{continue}{stmts}} > 0) ) {
            # { ... } block with next/last/redo/continue

            my $redo_label = Perlito5::Java::get_label();
            my $test_label = 'e.label_id != 0';
            $test_label = "e.label_id != $block_label && e.label_id != 0"
                if $block_label;

            my @continue;
            if ($self->{continue}) {

                # TODO - set up next/last/redo inside continue block

                local $Perlito5::JAVA_CAN_RETURN = 0;
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
        elsif ($has_local && !$last_statement) {
            # block with "local"
            push @str, 'PerlOp.cleanup_local(' . $local_label . ', PlCx.UNDEF);';
        }
        return ( @pre, @str );
    }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::CompUnit;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        local $Perlito5::JAVA_CAN_RETURN = 0;
        return Perlito5::Java::LexicalBlock->new(
                block => $self->{body},
                not_a_loop => 1,
            )->emit_java( $level + 1, $wantarray );
    }
    sub process_java_import_statement {
        my ($namespace, $annotation_ast) = @_;

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

        my $Java_class = Perlito5::Java::get_java_class_info();
        my $class = $namespace;
        # warn "Java class: $class\n";
        # TODO - add more information about the class

        # we need the parameter list as Perl data, so we need to evaluate the AST
        # - wrap the "list AST into a "hashref" AST
        my $args_ast = $annotation_ast;
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

        return $str;
    }
    sub emit_java_program {
        my ($comp_units, %options) = @_;
        $Perlito5::PKG_NAME = 'main';
        $Perlito5::THROW = 0;
        $Perlito5::THROW_RETURN = 0;
        my $level = 0;
        my $wantarray = 'statement';
        my $str;
        $str .= Perlito5::Compiler::do_not_edit("//");

        # look for special 'Java' packages
        Perlito5::Java::init_java_class();
        for my $ann ( @Perlito::ANNOTATION ) {
            $str .= process_java_import_statement(@$ann);
        }

        my @main;

        if ($options{'expand_use'}) {
            for my $k (keys %INC) {
                # serialize %INC
                push @main, Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        Perlito5::AST::Lookup->new(
                            obj => Perlito5::AST::Var->new(
                                '_decl' => "global",
                                '_namespace' => "main",
                                '_real_sigil' => "%",
                                'name' => "INC",
                                'namespace' => '',
                                'sigil' => "\$",
                            ),
                            index_exp => Perlito5::AST::Buf->new( buf => $k ),
                        ),
                        Perlito5::AST::Buf->new( buf => $INC{$k} ),
                    ],
                )->emit_java($level + 1, 'void') . ';';
            }
        }

        for my $comp_unit ( @$comp_units ) {
            my @str = $comp_unit->emit_java($level + 1, $wantarray);
            $str[-1] .= ";\n" if @str && !ref($str[-1]);
            push @main, @str;
        }
        if ($options{'expand_use'}) {
            my $Java_class = Perlito5::Java::get_java_class_info();
            $str .= join("",
                Perlito5::Java::Runtime->emit_java(
                    java_classes => $Java_class,
                    java_constants => [],  # \@Perlito5::Java::Java_constants,
                )
            );
        }

        my $main_class = "Main";
        $main_class = "LibPerl" if $Perlito5::BOOTSTRAP_JAVA_EVAL;

        $str .= Perlito5::Java::emit_wrap_java(-1,
               "class $main_class {",
               [ @Perlito5::Java::Java_constants,
                 "public static void main(String[] args) {",
                   [
                     ( $Perlito5::JAVA_EVAL ? "org.perlito.Perlito5.LibPerl.main( new String[]{} );" : ()),

                     "PlV.init(args);",
                     "int want = PlCx.VOID;",
                     "int return_context = PlCx.VOID;",
                     "PlArray List__ = new PlArray();",
                     "Exception ee = null;",
                     "try {",
                       [ @Perlito5::Java::Java_init,
                         @main,
                       ],
                     "}",
                     "catch(PlReturnException e) {",
                         # TODO - fix blocks and re-enable this error message
                         # [ 'ee = new PlDieException(new PlString("Can\'t return outside a subroutine"));' ],
                     "}",
                     "catch(PlNextException e) {",
                         [ 'ee = new PlDieException(new PlString("Can\'t \\"next\\" outside a loop block"));' ],
                     "}",
                     "catch(PlLastException e) {",
                         [ 'ee = new PlDieException(new PlString("Can\'t \\"last\\" outside a loop block"));' ],
                     "}",
                     "catch(PlRedoException e) {",
                         [ 'ee = new PlDieException(new PlString("Can\'t \\"redo\\" outside a loop block"));' ],
                     "}",
                     "catch(Exception e) {",
                         [ 'ee = e;' ],
                     "}",
                     # emit error messages
                     'if (ee != null) {',
                         [ # 'PlCORE.warn(PlCx.VOID, new PlArray(new PlString(ee.getMessage())));'
                           'ee.printStackTrace(System.err);',
                         ],
                     '}',
                     # execute END blocks

                     # TODO - catch error in END
                     # Perlito5::set_global_phase("END");
                     # eval {
                     #     $_->() for @Perlito5::END_BLOCK;
                     #     1;
                     # }
                     # or warn "$@\nEND failed--call queue aborted.\n"

                     'for (PlObject code : PlV.array_get("Perlito5::END_BLOCK")) {',
                         [ 'code.apply(PlCx.VOID, new PlArray());' ],
                     '}',
                     # return error code
                     'if (ee != null) {',
                         [ 'System.exit(1);' ],
                     '}',
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
                     "PlObject result = new PlString(functionName).apply(PlCx.LIST, list);",
                     "PlArray res = result instanceof PlArray ? (PlArray) result : new PlArray(result);",
                     "PlObject[] out = new PlObject[res.to_int()];",
                     "int i = 0;",
                     "for (PlObject s : res) {",
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
                     "PlObject result = new PlString(functionName).apply(PlCx.LIST, list);",
                     "PlArray res = result instanceof PlArray ? (PlArray) result : new PlArray(result);",
                     "PlObject[] out = new PlObject[res.to_int()];",
                     "int i = 0;",
                     "for (PlObject s : res) {",
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
        if ( length($v) > 19 || $v > 2**62 ) {
            # max value is 2**63 - 1
            if ( length($v) > 19 || $v >= 9223372036854775806.0 ) {
                return "new PlDouble(" . $v . ".0d)";
            }
            return "new PlInt(" . $v . "L)";
        }
        if ( $v >= -2 && $v < 0) {
            return "PlCx.MIN" . abs($v);
        }
        if ( $v >= 0 && $v <= 2) {
            return "PlCx.INT" . abs($v);
        }
        my $s = "new PlInt(" . $v . "L)";

        return Perlito5::Java::get_constant( "PlInt", $s );
    }
    sub emit_java_set {
        die "Can't modify constant item in scalar assignment";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Num;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $s = "new PlDouble(" . $self->{num} . "d)";

        return Perlito5::Java::get_constant( "PlDouble", $s );
    }
    sub emit_java_set {
        die "Can't modify constant item in scalar assignment";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Buf;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $s = "new PlStringConstant(" . Perlito5::Java::escape_string( $self->{buf} ) . ")";

        return Perlito5::Java::get_constant( "PlStringConstant", $s );
    }
    sub emit_java_set {
        die "Can't modify constant item in scalar assignment";
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
}

package Perlito5::AST::Block;
{
    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        local $Perlito5::THROW = 0;
        local $Perlito5::JAVA_CAN_RETURN = 0;
        my $body = Perlito5::Java::LexicalBlock->new(
            block       => $self->{stmts},
            block_label => $self->{label},
            continue    => $self->{continue},
        );
        my @str = $body->emit_java($level + 1, $wantarray);
        if ($Perlito5::THROW) {
            @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str, $wantarray );
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
            # (4,5,6)[0,2]
            return $self->{obj}->emit_java($level, 'list', 'lvalue') . '.aget_list_of_aliases('
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
            # %a[10, 20]
            # %$a[0, 2] ==> %{$a}[0,2]

            # "fix" the sigil type
            my $obj = $self->{obj};
            $obj->{sigil} = '@'
                if $obj->{sigil} eq '%';
            $obj->{code} = 'prefix:<@>'
                if $obj->{code} eq 'prefix:<%>';

            return $self->{obj}->emit_java($level) . '.aget_hash_list_of_aliases('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level)
                   . ')';
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a[0] ==> $a->[0]
            return Perlito5::AST::Call->new(
                'method' => 'postcircumfix:<[ ]>',
                'invocant' => $self->{obj}->{arguments}[0],
                'arguments' => $self->{index_exp},
            )->emit_java($level, $wantarray, $autovivification_type);
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
            return '((PlArray)' . $self->emit_java($level, 'list'). ').list_set('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . Perlito5::Java::to_list([$arguments], $level)
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
            # TODO - move this message to parser
            die "Can't modify index/value array slice in list assignment";
        }
        if ($localize) {
            return $self->emit_java_container($level) . '.aget_lvalue_local('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ').set('
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')';
        }
        my $s = Perlito5::Java::to_array_index( $self->{index_exp}, $level + 1, 'scalar' );

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # ${"Exporter::Cache"}[2]
            # $$a[0] ==> $a->[0]
            my $obj = Perlito5::Java::emit_java_autovivify( $self->{obj}->{arguments}[0], $level+1, 'array' );
            return $obj . '.aset('
                    . $s . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
                . ')';
        }

        # my $obj = Perlito5::Java::emit_java_autovivify( $self, $level+1, 'array' );
        # return $obj . '.aset(' 
        return $self->emit_java_container($level) . '.aset('
                    . $s . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
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

            return '((PlHash)' . $v->emit_java($level, 'scalar') . ").hget_list_of_aliases("
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

            return $v->emit_java($level) . '.hget_hash_list_of_aliases('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list([$self->{index_exp}], $level)
                   . ')'
        }
        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{aa} ==> $a->{aa}
            return Perlito5::AST::Call->new(
                'method' => 'postcircumfix:<{ }>',
                'invocant' => $self->{obj}->{arguments}[0],
                'arguments' => $self->{index_exp},
            )->emit_java($level, $wantarray, $autovivification_type);
        }
        my $index = Perlito5::AST::Lookup->autoquote($self->{index_exp});

        if (  $self->{obj}->isa('Perlito5::AST::Var')
           && $self->{obj}->{name} eq '+'
           && $self->{obj}->{_namespace} eq 'main'
           && $self->{obj}->{sigil} eq '$'
           )
        {
            # $+{aa}  => named capture from regex
            return 'PerlOp.regex_named_capture('
                . Perlito5::Java::to_native_str($index, $level)
            . ')';
        }

        return $self->emit_java_container($level) . '.' . $method . '('
                . Perlito5::Java::to_native_str($index, $level)
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
            return '((PlArray)' . $self->emit_java($level, 'list'). ').list_set('
                    . Perlito5::Java::to_context($wantarray) . ', '
                    . Perlito5::Java::to_list([$arguments], $level)
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
            # TODO - move this message to parser
            die "Can't modify index/value array slice in list assignment";
        }
        if ($localize) {
            return $self->emit_java_container($level) . '.hget_lvalue_local('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ').set('
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
            . ')';
        }

        if (  $self->{obj}->isa('Perlito5::AST::Apply')
           && $self->{obj}->{code} eq 'prefix:<$>'
           )
        {
            # $$a{x} ==> $a->{x}
            my $obj = Perlito5::Java::emit_java_autovivify( $self->{obj}->{arguments}[0], $level+1, 'hash' );
            return $obj . '.hset('
                    . Perlito5::Java::autoquote($self->{index_exp}, $level) . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
                . ')';
        }

        my $index = Perlito5::AST::Lookup->autoquote($self->{index_exp});
        return $self->emit_java_container($level) . '.hset('
                    . Perlito5::Java::to_native_str($index, $level) . ', '
                    . Perlito5::Java::to_scalar([$arguments], $level+1)
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
        '$' => '',
        '@' => '',
        '%' => '',
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
            if ( $wantarray eq 'runtime' || $wantarray eq 'return' ) {
                return '(' . Perlito5::Java::to_context($wantarray) . ' == PlCx.LIST'
                    . ' ? ' . $s
                    . ' : ' . $s . '.length_of_array()'
                    . ')';
            }
            return $s;
        }

        if ($sigil eq '$') {
            if ($self->{name} > 0) {
                # regex captures
                return 'PerlOp.regex_var(' . (0 + $self->{name}) . ')'
            }
            if ($self->{name} eq '&' || $self->{name} eq '`' || $self->{name} eq "'") {
                # regex match $&
                return 'PerlOp.regex_var(' . Perlito5::Java::escape_string($self->{name}) . ')'
            }
            if ($self->{name} eq '$') {
                # PID $$
                return 'PerlOp.getPID()'
            }
        }
        if ( $sigil eq '::' ) {
            return Perlito5::Java::escape_string( $namespace );
            # return Perlito5::AST::Buf->new( buf => $namespace )->emit_java($level, 'scalar');
        }

        my $index = Perlito5::Java::escape_string($namespace . '::' . $table->{$sigil} . $str_name);
        if ( $sigil eq '$' ) {
            return "PlV.sget$local(" . $index . ')';
        }
        if ( $sigil eq '*' ) {
            return "PlV.fget$local(" . $index . ')';
        }
        if ( $sigil eq '&' ) {
            my $namespace = $self->{namespace} || $Perlito5::PKG_NAME;
            # create a PlStringConstant
            my $sub = Perlito5::AST::Buf->new( buf => $namespace . '::' . $str_name )->emit_java($level, 'scalar');
            return $sub . '.apply(' . Perlito5::Java::to_context($wantarray) . ', List__)';
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
            if (!defined($str_name)) {
                # %Module::
                return "PerlOp.getSymbolTable(" . $index . ')';
            }
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
            # TODO - return in the right context
            # TODO - local()-ize if needed
            return $s . ".set(" . Perlito5::Java::to_list([$arguments], $level+1) . ')';
            # if ( $wantarray eq 'runtime' ) {
            #     return '(want'
            #         . ' ? ' . $s
            #         . ' : ' . $s . '.to_long()'
            #         . ')';
            # }
            # return $s;
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
            return "PlV.sset$local(" . $index . ', ' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')';
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
            return "PlV.glob_set$local(" . $index . ', '
                . Perlito5::Java::to_scalar([$arguments], $level+1) . ', '
                . Perlito5::Java::escape_string($Perlito5::PKG_NAME)
                . ')';
        }
        if ( $sigil eq '&' ) {
            # return 'PlV.apply(' . $index . ', ' . Perlito5::Java::to_context($wantarray) . ', List__)';
        }
        die "don't know how to assign to variable ", $sigil, $self->name;
    }

    sub emit_java_global_set_alias {
        my ($self, $arguments, $level, $wantarray, $localize) = @_;
        die "can't localize emit_java_global_set_alias()" if $localize;
        my $str_name = $self->{name};
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $namespace = $self->{namespace} || $self->{_namespace};
        if ($sigil eq '$' && $self->{name} > 0) {
            # TODO - for $1 (...) {} is valid perl
            die "not implemented emit_java_global_set_alias() for regex capture";
        }
        my $index = Perlito5::Java::escape_string($namespace . '::' . $table->{$sigil} . $str_name);
        $arguments = Perlito5::Java::to_scalar([$arguments], $level+1)
            if ref($arguments);
        return "PlV.sset_alias(" . $index . ', ' . $arguments . ")" if $sigil eq '$';
        return "PlV.aset_alias(" . $index . ', ' . $arguments . ")" if $sigil eq '@';
        return "PlV.hset_alias(" . $index . ', ' . $arguments . ")" if $sigil eq '%';
        die "can't emit_java_global_set_alias() for sigil '$sigil'";
    }

    sub emit_java {
        my ($self, $level, $wantarray) = @_;
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        my $decl_type = $self->{_decl} || 'global';
        if ( $decl_type ne 'my' && $decl_type ne 'state' ) {
            return $self->emit_java_global($level, $wantarray);
        }
        my $str_name = $table->{$sigil} . $self->{name} . "_" . $self->{_id};

        $str_name = $Perlito5::Java::Java_var_name{$self->{_id}}
            if exists $Perlito5::Java::Java_var_name{$self->{_id}};

        if ( $sigil eq '@' ) {
            if ( $wantarray eq 'scalar' ) {
                return $self->emit_java($level, 'list') . '.length_of_array()';
            }
            if ( $wantarray eq 'runtime' || $wantarray eq 'return' ) {
                return '(' . Perlito5::Java::to_context($wantarray) . ' == PlCx.LIST'
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
        if ( $decl_type ne 'my' && $decl_type ne 'state' ) {
            return $self->emit_java_global_set($arguments, $level, $wantarray);
        }
        my $open  = $wantarray eq 'void' || $wantarray eq 'statement' ? '' : '(';
        my $close = $wantarray eq 'void' || $wantarray eq 'statement' ? '' : ')';
        my $sigil = $self->{_real_sigil} || $self->{sigil};
        if ( $sigil eq '$' ) {
            my $id = $self->{_id};
            my $Java_var = Perlito5::Java::get_java_var_info();
            my $type = $Java_var->{ $id }{type} || 'PlLvalue';
            if ($type ne 'PlLvalue') {
                # set a typed variable - there is no .set() method
                # the arguments are not boxed
                return $self->emit_java($level) . ' = ' . Perlito5::Java::to_native_args([$arguments]);
            }
            return $self->emit_java($level) . '.set(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')'
        }
        if ( $sigil eq '@' ) {

            if ($self->{sigil} eq '$#') {
                $self->{sigil} = '@';
                return $open . $self->emit_java($level) . '.set_end_of_array_index(' . Perlito5::Java::to_scalar([$arguments], $level+1) . ')' . $close
            }

            return $self->emit_java($level) . '.set(' . Perlito5::Java::to_list([$arguments], $level+1) . ')'
        }
        if ( $sigil eq '%' ) {
            return $self->emit_java($level) . '.set('
            .   Perlito5::Java::to_context($wantarray) . ', '
            .   Perlito5::Java::to_list([$arguments], $level+1, 'hash')
            . ')';
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

            return if $Java_var->{ $id };   # done

            $Java_var->{ $id } = { id => $id, type => $type };
        }

        if ($self->{decl} eq 'our') {
            return '';
        }
        if ($self->{decl} eq 'local') {
            return '';
        }
        if ($self->{decl} eq 'my' || $self->{decl} eq 'state') {
            if ($self->{var}->sigil eq '%') {
                return 'PlHash ' . $self->{var}->emit_java($level) . ' = new PlHash();';
            }
            elsif ($self->{var}->sigil eq '@') {
                return 'PlArray ' . $self->{var}->emit_java($level) . ' = new PlArray();';
            }
            else {
                my $Java_class = Perlito5::Java::get_java_class_info();
                my $java_type = $Java_class->{$type}{java_type} || 'PlLvalue';
                if( $java_type eq 'PlLvalue' ) {
                    return "${java_type} " . $self->{var}->emit_java($level) . " = new ${java_type}();";
                } else {
                    return "${java_type} " . $self->{var}->emit_java($level) . ";";
                }
            }
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
                . '.' . $method . '('
                .       Perlito5::Java::to_array_index( $self->{arguments}, $level + 1, 'scalar' )
                . ')';
        }
        if ( $meth eq 'postcircumfix:<{ }>' ) {
            my $method = $autovivification_type || 'hget';
            $method = 'hget_scalarref' if $autovivification_type eq 'scalar';
            $method = 'hget_arrayref'  if $autovivification_type eq 'array';
            $method = 'hget_hashref'   if $autovivification_type eq 'hash';
            $method = 'hget_lvalue'    if $autovivification_type eq 'lvalue';
            return Perlito5::Java::emit_java_autovivify( $self->{invocant}, $level, 'hash' )
                . '.' . $method . '('
                .       Perlito5::Java::to_native_str(Perlito5::AST::Lookup->autoquote($self->{arguments}), $level + 1, 'list')
                . ')';
        }
        if  ($meth eq 'postcircumfix:<( )>')  {
            # $x->()
            my $invocant;

            if (  ref( $self->{invocant} ) eq 'Perlito5::AST::Var' && $self->{invocant}{sigil} eq '::' ) {
                if ( $self->{invocant}{namespace} eq '__SUB__' || $self->{invocant}{namespace} eq 'CORE::__SUB__' ) {
                    # __SUB__->()
                    $invocant = 'this.getCurrentSub()';     # "this" is the closure
                }
                else {
                    # x->()
                    my $fullname = $self->{invocant}{namespace};
                    if ($fullname !~ /::/) {
                        $fullname = $Perlito5::PKG_NAME . '::' . $fullname;
                    }
                    $invocant = Perlito5::AST::Buf->new( buf => $fullname )->emit_java($level, 'scalar');
                }
            }
            elsif (  ref( $self->{invocant} ) eq 'Perlito5::AST::Int' ) {
                # 4->()
                $invocant = Perlito5::AST::Buf->new( buf => $Perlito5::PKG_NAME . '::' . $self->{invocant}{int} )->emit_java($level, 'scalar');
            }
            else {
                # $x->()
                $invocant = $self->{invocant}->emit_java($level, 'scalar');
            }

            return $invocant . '.apply('
                        . Perlito5::Java::to_context($wantarray) . ', '
                        . Perlito5::Java::to_list($self->{arguments})
                    . ')';
        }
        if  (substr($meth, 0, 7) eq 'SUPER::')  {
            # $x->SUPER::meth
            my $method = substr($meth, 7);
            if ($method) {
                $method = Perlito5::Java::escape_string($method);
                return 'PerlOp.callSuper('
                         . $method . ', ' 
                         . Perlito5::Java::pkg . ', '
                         . Perlito5::Java::to_method_call_param_list($self->{invocant}, $self->{arguments}, $level + 1) . ', '
                         . Perlito5::Java::to_context($wantarray)
                  . ')'
            }
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
            # create a PlStringConstant
            $meth = Perlito5::AST::Buf->new( buf => $meth )->emit_java($level, 'scalar');
        }

        return 'PerlOp.call('
                   . $meth . ', ' 
                   . Perlito5::Java::to_method_call_param_list($self->{invocant}, $self->{arguments}, $level + 1) . ', '
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
                        . Perlito5::Java::to_native_str(Perlito5::AST::Lookup->autoquote($self->{arguments}), $level + 1, 'list') . ', '
                        . Perlito5::Java::to_scalar([$arguments], $level+1)
                    . ')';
        }
        die "don't know how to assign to method ", $self->{method};
    }
    sub emit_java_get_decl { () }
    sub emit_java_has_regex { () }
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
            : Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, not_a_loop => 1 );
        my $otherwise =
              ref($self->{otherwise}) ne 'Perlito5::AST::Block'
            ? $self->{otherwise}  # may be undef
            : (!@{ $self->{otherwise}->stmts })
            ? undef
            : Perlito5::Java::LexicalBlock->new( block => $self->{otherwise}->stmts, not_a_loop => 1 );
 
        push @str, 'if (' . Perlito5::Java::to_boolean($cond, $level + 1) . ') {';
        if ($body) {
            push @str, [ $body->emit_java( $level + 1, $wantarray ) ];
        }
        push @str, '}';
        if ($otherwise) {
            if ( @{ $otherwise->{block} } == 1 
               && ref($otherwise->{block}[0]) eq 'Perlito5::AST::If'
               )
            {
                push @str, ( 'else', $otherwise->{block}[0]->emit_java( $level, $wantarray ) );
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
                    Perlito5::AST::Var::SCALAR_ARG(),
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
            : Perlito5::Java::LexicalBlock->new( block => $self->{body}->stmts, not_a_loop => 1 );
        push @{ $body->{block} }, $next; 

        push @str, 'if (' . Perlito5::Java::to_boolean($cond, $level + 1) . ') {';
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
            $expression =  Perlito5::Java::to_boolean($cond, $level + 1);    
        }
        if ($expression eq 'false') {
            # no-op
            return 'PerlOp.statement();';
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
                @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str, $wantarray );
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
        for my $expr ( @$cond ) {
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
                    . ( $self->{cond}[1] ? Perlito5::Java::to_boolean($self->{cond}[1], $level + 1) . '; '  : '; ' )
                    . ( $self->{cond}[2] ? $self->{cond}[2]->emit_java($level + 1) . ' '   : ''  )
                  . ') {',
                      [
                        # $v->emit_java($level + 1) . ".set(item);",
                        (Perlito5::Java::LexicalBlock->new( block => $body, block_label => $self->{label} ))->emit_java($level + 2, $wantarray),
                      ],
                '}';
        }
        else {
            my $local_label = Perlito5::Java::get_label();
            my $cond = $self->{cond};
            my $loop_expression;
            my $loop_expression_is_integer = 0;
            if ( $cond->isa( 'Perlito5::AST::Apply' ) && $cond->{code} eq 'infix:<..>' ) {
                my ($arg1, $arg2) = @{ $cond->{arguments} };

                if ($arg1->isa('Perlito5::AST::Int') && $arg2->isa('Perlito5::AST::Int')) {
                    $loop_expression_is_integer = 1;
                    $loop_expression = 
                          'long ' . $local_label . ' = ' . $arg1->{int} . '; '
                        . $local_label . ' <= ' . $arg2->{int} . '; '
                        . $local_label . '++';
                }
                else {
                    $loop_expression = 'PlObject ' . $local_label
                        . ' : new PerlRange('
                              . $arg1->emit_java($level + 1) . ', '
                              . $arg2->emit_java($level + 1)
                        .     ')';
                }
            }
            else {
                # TODO - optimization - use to_list() when the topic doesn't need to mutate
                $loop_expression = 'PlObject ' . $local_label
                    . ' : ' . Perlito5::Java::to_param_list([$cond], $level + 1);
            }

            my $decl = '';
            my $v = $self->{topic};
            if ($v->{decl}) {
                $decl = $v->{decl};
                $v    = $v->{var};
            }
            else {
                $decl = $v->{_decl} || 'global';
            }
            my $namespace = $v->{namespace} || $v->{_namespace} || $Perlito5::PKG_NAME;
            my $s;
            if ($decl eq 'my' || $decl eq 'state' ) {
                # TODO - use PlObject for topic, because:
                #   - less memory
                #   - arguments are r/o when needed
                my $loop_init;
                if ($loop_expression_is_integer) {
                    $loop_init = 'PlLvalue ' . $v->emit_java($level + 1) . ' = new PlLvalue(' . $local_label . ')';
                }
                else {
                    $loop_init = 'PlLvalue ' . $v->emit_java($level + 1) . " = (PlLvalue)$local_label";
                }
                push @str,
                        'for (' . $loop_expression . ') {',
                          [ $loop_init . ";",
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
                # localize variable
                my $local_label2 = Perlito5::Java::get_label();
                my $loop_init;
                if ($loop_expression_is_integer) {
                    $loop_init = $v->emit_java($level, 'scalar', 'lvalue') . '.set(' . $local_label . ')';
                }
                else {
                    $loop_init = $v->emit_java_global_set_alias( "(PlLvalue)$local_label", $level + 1 );
                }
                push @str, 'int ' . $local_label2 . ' = PerlOp.local_length();';
                push @str, $v->emit_java_global($level + 1, 'scalar', 1) . ";";
                push @str,
                        'for (' . $loop_expression . ') {',
                          [ $loop_init . ";",
                            Perlito5::Java::LexicalBlock->new(
                                block => $body,
                                block_label => $self->{label},
                                continue => $self->{continue},
                            )->emit_java($level + 2, $wantarray),
                          ],
                        '}';
                push @str, 'PerlOp.cleanup_local(' . $local_label2 . ', PlCx.UNDEF);';
            }
        }
        if ($Perlito5::THROW) {
            @str = Perlito5::Java::emit_wrap_last_exception_java( $self, \@str, $wantarray );
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
        # Statevar transformation. See lib/Perlito5/Macro.pm
        if (my $node = $self->maybe_rewrite_statevars()) {
            return $node->emit_java($level, $wantarray);
        }
        local $Perlito5::THROW;
        local $Perlito5::THROW_RETURN if !$self->{_do_block};
        my $prototype = defined($self->{sig}) 
                        ? 'new PlString(' . Perlito5::Java::escape_string($self->{sig}) . ')'
                        : 'PlCx.UNDEF';

        my $attributes = $self->{attributes} || [];
        my $is_lvalue;
        for (@$attributes) {
            $is_lvalue = 1 if $_->[0] eq 'lvalue';
        }
        # warn "is_lvalue" if $is_lvalue;

        my $is_defined = 'true';
        if (!defined $self->{block}) {
            # this is a predeclaration
            $is_defined = 'false';
        }

        my $outer_sub;
        $outer_sub = 'this.getCurrentSub()' if $Perlito5::Java::is_inside_subroutine;

        my $sub_ref = Perlito5::Java::get_label();
        local $Perlito5::Java::is_inside_subroutine = 1;
        my $block = Perlito5::Java::LexicalBlock->new( block => $self->{block}{stmts}, not_a_loop => 1 );

        # get list of captured variables, including inner blocks
        my @captures_ast = @Perlito5::CAPTURES;

        {
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

            my %seen = map { $_->{_id} => 1 } @captures_ast;

            my @more = (
                grep { ! $seen{ $_->{_id} } }
                map  { $capture{$_} }
                sort keys %capture );
            push @captures_ast, @more;
        }

        local @Perlito5::CAPTURES = @captures_ast;

        my @captures_java = map { $_->emit_java( $level, 'list' ) } @captures_ast;

        # set the new variable names inside the closure
        local %Perlito5::Java::Java_var_name;
        my $i = 0;
        for (@captures_ast) {
            my $capture_name = 'this.env[' . $i . ']';
            my $sigil = $_->{_real_sigil} || $_->{sigil};
            if ($sigil eq '$') {
                $capture_name = "((PlLvalue)$capture_name)";
            }
            elsif ($sigil eq '@') {
                $capture_name = "((PlArray)$capture_name)";
            }
            elsif ($sigil eq '%') {
                $capture_name = "((PlHash)$capture_name)";
            }
            $Perlito5::Java::Java_var_name{ $_->{_id} } = $capture_name;
            $i++;
        }

        my @js_block;
        if ($self->{_do_block}) {
            # do-block
            local $Perlito5::JAVA_CAN_RETURN = 0;
            @js_block = $block->emit_java( $level + 3, 'runtime' );
        }
        elsif ($self->{_eval_block}) {
            # eval-block
            $block->{top_level} = 1;
            $block->{eval_block} = 1;
            local $Perlito5::JAVA_CAN_RETURN = 1;
            @js_block = $block->emit_java( $level + 3, 'runtime' ),
        }
        else {
            local $Perlito5::JAVA_CAN_RETURN = 1;
            $block->{top_level} = 1;
            @js_block = $block->emit_java( $level + 3, 'runtime' );
        }

        if (!@js_block) {
            push @js_block, 'return PerlOp.context(want);';
        }

        my @closure_args = (
              $prototype,
              "new PlObject[]{ " . join(', ', @captures_java) . " }",
              Perlito5::Java::pkg,
              $is_defined,
        );
        if (($self->{_do_block} || $self->{_eval_block}) && $outer_sub) {
            push @closure_args, $outer_sub;
        }

        my @perl_pos;
        if ($self->{pos}) {
            @perl_pos = (
                  "public String perlFileName() {",
                    [ "return " . Perlito5::Java::escape_string( $self->{pos}{file} ) . ";",
                    ],
                  "}",
                  "public Integer perlLineNumber() {",
                    [ "return " . (0 + $self->{pos}{line}) . ";",
                    ],
                  "}",
            );
        }

        my $method_decl;
        if ($self->{_do_block}) {
            $method_decl = "public PlObject apply_do_block(int want, int return_context, PlArray List__)";
        }
        else {
            $method_decl = "public PlObject apply(int want, PlArray List__)";
            unshift @js_block, "int return_context = want;";
        }

        my @s = (
            "new PlClosure(" . join( ", ", @closure_args ) . ") {",
                [
                  @perl_pos,
                  "public StackTraceElement firstLine() {",
                    [ "return PlCx.mainThread.getStackTrace()[1];",
                    ],
                  "}",
                  $method_decl . " {",
                    [ @js_block ],
                  "}",
                  "public StackTraceElement lastLine() {",
                    [ "return PlCx.mainThread.getStackTrace()[1];",
                    ],
                  "}",
                ],
            "}",
        );

        if ( $self->{name} ) {
            return Perlito5::Java::emit_wrap_java($level,
                'PlV.cset(',
                    [ Perlito5::Java::escape_string( $self->{namespace} . '::' . $self->{name} ) . ",",
                      @s,
                    ],
                ');',
            );
        }
        else {
            return "" . Perlito5::Java::emit_wrap_java($level, @s);
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

1;

=begin

=head1 NAME

Perlito5::Java::Emit - Code generator for Perlito Perl5-in-Java

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

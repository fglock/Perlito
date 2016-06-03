package Perlito5::CompileTime::Dumper;

use strict;

sub generate_eval_string {
    my ($source) = @_;
    # print STDERR "[[[ $source ]]]\n";
    my $m = Perlito5::Grammar::exp_stmts($source, 0);
    my $block = Perlito5::AST::Block->new( stmts => Perlito5::Match::flat($m) );

    ## TODO: enable emit_compile_time()
    ##
    ##  $ node perlito5.js -I src5/lib  t5/op/pow.t
    ##  [[[ { package main;
    ##  {
    ##      chdir 't' if -d 't';
    ##      @INC = '../lib';
    ##      require './test.pl';
    ##  } }; 1 ]]]
    ##  Error in BEGIN block: TypeError: Cannot call method 'p5aget' of undefined
    ##
    # my @data = $block->emit_compile_time;

    my @data = $block->emit_perl5;
    my $out = [];
    Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
    my $source_new = join( '', @$out ), ";1\n";
    # print STDERR "[[[ $source_new ]]]\n";
    return $source_new;
}

# $ perl -Isrc5/lib -e ' use strict; use Perlito5::CompileTime::Dumper; use Perlito5::AST; use Data::Dumper; my $s = [ 1,2.1,{a=>4},undef,\6 ]; $s->[3]=$s->[2]; my $seen = {}; print Dumper Perlito5::CompileTime::Dumper::_dump_to_ast( $s, " ", $seen, "s" ); sub Perlito5::Dumper::escape_string { @_ }'

sub _dump_to_ast {
    my ($obj, $seen, $pos) = @_;

    return Perlito5::AST::Apply->new(code => 'undef', arguments => []) if !defined $obj;

    my $ref = ref($obj);
    if (!$ref) {
        if ( 0+$obj eq $obj ) {
            return Perlito5::AST::Int->new(int => $obj) if int($obj) == $obj;
            return Perlito5::AST::Num->new(num => $obj);
        }
        return Perlito5::AST::Buf->new(buf => $obj);
    }

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;
        
    if ($ref eq 'ARRAY') {
        my @out;
        for my $i ( 0 .. $#$obj ) {
            # TODO - move self-referencing outside the expression
            my $here = # $pos . '->[' . $i . ']';
                Perlito5::AST::Index::INDEX( $pos, Perlito5::AST::Int->new(int => $i) );
            push @out, _dump_to_ast($obj->[$i], $seen, $here);
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<[ ]>', arguments => \@out);
    }
    elsif ($ref eq 'HASH') {
        my @out;
        for my $i ( sort keys %$obj ) {
            # TODO - move self-referencing outside the expression
            my $here = # $pos . '->{' . $i . '}';
                Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) );
            push @out, Perlito5::AST::Apply->new(
                code => 'infix:<=>>',
                arguments => [
                    Perlito5::AST::Buf->new(buf => $i),
                    _dump_to_ast($obj->{$i}, $seen, $here),
                ],
            );
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<{ }>', arguments => \@out);
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
            # TODO - move self-referencing outside the expression if needed
        return Perlito5::AST::Apply->new(
            code => 'prefix:<\\>',
            arguments => [_dump_to_ast($$obj, $seen, $pos)]
        );
    }
    elsif ($ref eq 'CODE') {
        # TODO

        # get the closed variables - see 'Sub' in Perl5 emitter
        my $closure_flag = bless {}, "Perlito5::dump";
        my $captures = $obj->($closure_flag) // {};

        my @vars;
        my $ast;
        my $source;
        my $sub_name;
        my $package = $captures->{__PKG__};
        if ($package) {
            push @vars, # "package $package;"
                Perlito5::AST::Apply->new(
                    'code'      => 'package',
                    'namespace' => $package,
                    'arguments' => [],
                );
        }
        for my $var (sort keys %$captures) {
            next if $var eq "__PKG__";
            if ($var eq '__SUB__') {
                my $sub_id = $captures->{$var};
                $ast = $Perlito5::BEGIN_SUBS{$sub_id};

                $sub_name = $ast->{namespace} . "::" . $ast->{name}
                    if $ast->{name};

                # my @data = $ast->emit_perl5();
                # my $out = [];
                # Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                $source = $ast;
            }
            else {
                push @vars, 
                    # 'my ' . $var . ' = ' . _dump_to_ast_deref($captures->{$var}, $seen, $pos) . '; ';
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Decl->new(
                                'attributes' => [],
                                'decl' => 'my',
                                'type' => '',
                                'var' => $var,      # TODO
                            ),
                            _dump_to_ast_deref($captures->{$var}, $seen, $pos),
                        ],
                    );
            }
        }
        # say "_dump_to_ast: source [[ $source ]]";
        return Perlito5::AST::Apply->new(
            code => 'do',
            arguments => [
                Perlito5::AST::Block->new(
                    stmts => [
                        # TODO
                        @vars,
                        $source,
                        ( $sub_name
                          ? '\\&' . $sub_name    # return pointer to subroutine
                          : ''
                        ),
                    ],
                ),
            ],
        );
    }

    # TODO find out what kind of reference this is (ARRAY, HASH, ...)
    # local $@;
    # eval {
    #     my @data = @$obj;
    #     say "is array";
    #     return 'bless(' . "..." . ", '$ref')";
    # }
    # or eval {
    #     $@ = '';
    #     my %data = %$obj;
    #     say "is hash";
    #     return 'bless(' . "..." . ", '$ref')";
    # };
    # $@ = '';
    
    # assume it's a blessed HASH
    
    my @out;
    for my $i ( sort keys %$obj ) {
        my $here = # $pos . '->{' . $i . '}';
                Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) );
        push @out, Perlito5::AST::Apply->new(
            code => 'infix:<=>>',
            arguments => [
                Perlito5::AST::Buf->new(buf => $i),
                _dump_to_ast($obj->{$i}, $seen, $here),
            ],
        );
    }
    return Perlito5::AST::Apply->new(
        code => 'bless',
        arguments => [
            Perlito5::AST::Apply->new(code => 'circumfix:<{ }>', arguments => \@out),
            Perlito5::AST::Buf->new(buf => $ref),
        ],
    );
}

sub _dump_to_ast_deref {
    my ($obj, $seen, $pos) = @_;
    my $ref = ref($obj);
    return _dump_to_ast(@_) if !$ref;
    if ($ref eq 'ARRAY') {
        return '()' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = # $pos . '->[' . $i . ']';
                Perlito5::AST::Index::INDEX( $pos, Perlito5::AST::Int->new(int => $i) );
            push @out, _dump_to_ast($obj->[$i], $seen, $here);
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<( )>', arguments => \@out);
    }
    elsif ($ref eq 'HASH') {
        return '()' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = # $pos . '->{' . $i . '}';
                Perlito5::AST::Lookup::LOOKUP( $pos, Perlito5::AST::Buf->new(buf => $i) );
            push @out, Perlito5::AST::Apply->new(
                code => 'infix:<=>>',
                arguments => [
                    Perlito5::AST::Buf->new(buf => $i),
                    _dump_to_ast($obj->{$i}, $seen, $here),
                ],
            );
        }
        return Perlito5::AST::Apply->new(code => 'circumfix:<( )>', arguments => \@out);
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        return _dump_to_ast($$obj, $seen, $pos);
    }
    return _dump_to_ast($obj, $seen, $pos);
}


sub dump_to_AST_after_BEGIN {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program
    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";

    for my $name (sort keys %$scope) {
        my $sigil = substr($name, 0, 1);
        my $item = $scope->{$name};
        if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
            # TODO
            # _dump_global($item, $seen, $dumper_seen, $vars, $tab);
            push @$vars, "# don't know how to initialize subroutine $name";
            next;
        }

        # TODO - emit lexicals
        next
            if substr($name, 1, 2) eq "C_";

        if (substr($name, 7, 1) lt 'A') {
            # encode special variable names like $main::" to ${'main::"'}
            $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
        }
        my $ast = $item->{ast};
        if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
            # "our" variables are lexical aliases; we want the original global variable name
            $name =
                  ($ast->{_real_sigil} || $ast->{sigil})
                . ($ast->{namespace} || $ast->{_namespace} || "C_")
                . "::" . $ast->{name};
        }
        if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
            my $value = eval($name);
            next if !defined($value);
            push @$vars, # "$name = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            $ast,
                            _dump_to_ast( $value, "  ", $dumper_seen, $name ),
                        ],
                    );
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && ($sigil eq '@' || $sigil eq '%')) {
            my $value = eval("\\" . $name);
            # my $bareword = substr($name, 1);
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            _dump_to_ast( $value, "  ", $dumper_seen, '\\' . $name ),
                        ],
                    );
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
            # *mysub = sub {...}
            my $bareword = substr($name, 1);

            if (exists &{$bareword}) {
                my $sub = \&{$bareword};
                my $dump = _dump_to_ast($sub, '  ', $dumper_seen, '\\&' . $bareword);
                push @$vars, "*$bareword = ", $dump;
            }
            if (defined ${$bareword}) {
                my $sub = \${$bareword};
                my $dump = _dump_to_ast($sub, '  ', $dumper_seen, '\\$' . $bareword);
                push @$vars, "*$bareword = ", $dump;
            }
            if (@{$bareword}) {
                my $sub = \@{$bareword};
                my $dump = _dump_to_ast($sub, '  ', $dumper_seen, '\\@' . $bareword);
                push @$vars, "*$bareword = ", $dump;
            }
            if (keys %{$bareword}) {
                my $sub = \%{$bareword};
                my $dump = _dump_to_ast($sub, '  ', $dumper_seen, '\\%' . $bareword);
                push @$vars, "*$bareword = ", $dump;
            }

        }
        else {
            push @$vars, "# don't know how to initialize variable $name";
        }
    }
    return \@$vars;
}

sub _dumper {
    my ($obj, $tab, $seen, $pos) = @_;

    return 'undef' if !defined $obj;

    my $ref = ref($obj);
    return Perlito5::Dumper::escape_string($obj) if !$ref;

    my $as_string = "$obj";
    return $seen->{$as_string} if $seen->{$as_string};
    $seen->{$as_string} = $pos;
        
    my $tab1 = $tab . '    ';

    if ($ref eq 'ARRAY') {
        return '[]' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "[\n", @out, $tab, ']');
    }
    elsif ($ref eq 'HASH') {
        return '{}' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "{\n", @out, $tab, '}');
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        return "\\" . _dumper($$obj, $tab1, $seen, $pos);
    }
    elsif ($ref eq 'CODE') {
        # TODO

        # get the closed variables - see 'Sub' in Perl5 emitter
        my $closure_flag = bless {}, "Perlito5::dump";
        my $captures = $obj->($closure_flag) // {};

        my @vars;
        my $ast;
        my $source;
        my $sub_name;
        my $package = $captures->{__PKG__};
        push @vars, "package $package;"
            if $package;
        for my $var (sort keys %$captures) {
            next if $var eq "__PKG__";
            if ($var eq '__SUB__') {
                my $sub_id = $captures->{$var};
                $ast = $Perlito5::BEGIN_SUBS{$sub_id};

                $sub_name = $ast->{namespace} . "::" . $ast->{name}
                    if $ast->{name};;

                my @data = $ast->emit_perl5();
                my $out = [];
                Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                $source = join( '', @$out ) . ";";
            }
            else {
                push @vars, 
                    'my ' . $var . ' = ' . _dumper_deref($captures->{$var}, $tab1, $seen, $pos) . '; ';
            }
        }
        # say "_dumper: source [[ $source ]]";
        return join('',
            'do { ',
                @vars,
                $source,
                ( $sub_name
                  ? '\\&' . $sub_name    # return pointer to subroutine
                  : ''
                ),
            '}'
        );
    }

    # TODO find out what kind of reference this is (ARRAY, HASH, ...)
    # local $@;
    # eval {
    #     my @data = @$obj;
    #     say "is array";
    #     return 'bless(' . "..." . ", '$ref')";
    # }
    # or eval {
    #     $@ = '';
    #     my %data = %$obj;
    #     say "is hash";
    #     return 'bless(' . "..." . ", '$ref')";
    # };
    # $@ = '';
    
    # assume it's a blessed HASH
    
    my @out;
    for my $i ( sort keys %$obj ) {
        my $here = $pos . '->{' . $i . '}';
        push @out, 
            $tab1,
            "'$i' => ",
            _dumper($obj->{$i}, $tab1, $seen, $here), 
            ",\n";
    }
    return join('', "bless({\n", @out, $tab, "}, '$ref')");
}

sub _dumper_deref {
    my ($obj, $tab, $seen, $pos) = @_;
    my $ref = ref($obj);
    return _dumper(@_) if !$ref;
    my $tab1 = $tab . '    ';
    if ($ref eq 'ARRAY') {
        return '()' unless @$obj;
        my @out;
        for my $i ( 0 .. $#$obj ) {
            my $here = $pos . '->[' . $i . ']';
            push @out, 
                $tab1,
                _dumper($obj->[$i], $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "(\n", @out, $tab, ')');
    }
    elsif ($ref eq 'HASH') {
        return '()' unless keys %$obj;
        my @out;
        for my $i ( sort keys %$obj ) {
            my $here = $pos . '->{' . $i . '}';
            push @out, 
                $tab1,
                "'$i' => ",
                _dumper($obj->{$i}, $tab1, $seen, $here), 
                ",\n";
        }
        return join('', "(\n", @out, $tab, ')');
    }
    elsif ($ref eq 'SCALAR' || $ref eq 'REF') {
        return _dumper($$obj, $tab1, $seen, $pos);
    }

    my @out;
    for my $i ( sort keys %$obj ) {
        my $here = $pos . '->{' . $i . '}';
        push @out, 
            $tab1,
            "'$i' => ",
            _dumper($obj->{$i}, $tab1, $seen, $here), 
            ",\n";
    }
    return join('', "bless({\n", @out, $tab, "}, '$ref')");
}

sub _dump_global {
    my ($item, $seen, $dumper_seen, $vars, $tab) = @_;

    if (ref($item) eq 'Perlito5::AST::Sub') {
        my $n = $item->{namespace} . "::" . $item->{name};
        if (!$seen->{$n}) {
            push @$vars, $tab . "sub $n = " . _dumper( $item, "  ", $dumper_seen, $n ) . ";\n";
            $seen->{$n} = 1;
        }
    }
    elsif (ref($item) eq 'Perlito5::AST::Var') {
        my $n = $item->{sigil} . $item->{namespace} . "::" . $item->{name};
        if (!$seen->{$n}) {
            if ($item->{sigil} eq '$') {
                push @$vars, $tab . "$n = " . _dumper( eval $n, "  ", $dumper_seen, $n ) . ";\n";
            }
            elsif ($item->{sigil} eq '@' || $item->{sigil} eq '%') {
                my $ref = "\\$n";
                my $d = _dumper( eval $ref, $tab . "  ", $dumper_seen, $ref );
                if ($d eq '[]' || $d eq '{}') {
                    push @$vars, $tab . "$n = ();\n"
                }
                else {
                    push @$vars, $tab . "$n = " . $item->{sigil} . "{" . $d . "};\n";
                }
            }
            elsif ($item->{sigil} eq '*') {
                # TODO - look for aliasing
                #   *v1 = \$v2
                push @$vars, $tab . "# $n\n";
                for (qw/ $ @ % /) {
                    local $item->{sigil} = $_;
                    _dump_global($item, $seen, $dumper_seen, $vars, $tab);
                }
            }
            $seen->{$n} = 1;
        }
    }
}


# TODO
#   - move global variables from SCOPE to GLOBAL
#   - analyze the list of captures and resolve shared lexicals
#       - when 2 closures share code, we need to decide if the captured
#         variables are shared or not.
#         Closures can have un-shared variables if the closure is created
#         in a loop inside BEGIN
#       - shared lexicals can be obtained from the data in $Perlito5::SCOPE

#   - this Perl warning should probably be fatal in Perlito5:
#       Variable "$z" will not stay shared
#     the "perl" behaviour is hard to replicate - see misc/compile-time.
#     Example:
#
#       use strict; use warnings;
#       sub z0 {
#           my $z = shift;
#           sub z1 { $z }
#       }
#
#     alternately:
#       sub z0 {
#           my $z = shift;
#           BEGIN { *z1 = sub { $z } }
#       }
#
#       sub z0 {
#           my $z = shift;
#           BEGIN { $z }
#       }
#
#     the variable '$z' will be shared only on the 'first' execution of 'z0';
#     subsequent executions of 'z0' will create a new pad.
#
#     BEGIN blocks inside loops have a similar problem, but don't
#     generate a warning in "perl".


# problems to look for:
#   - closures created in loops in BEGIN blocks share variable names,
#       but the variables belong to different "pads" / activation records"
#   - closures created after variable redefinition don't share variables
#   - lexical variables can be shared across closures
#   - our variables
#   - in order to conserve memory at compile-time,
#       create subroutine stubs that expand into instrumented code when called
#   - bootstrapping rewrites Perlito5::* subroutines and globals, probably breaking the compiler.
#       special-casing the Perlito5 namespace at compile-time should work around the problem.

# Note
#   - variables with 'undef' value don't need to be processed,
#     because the runtime will re-create them


sub emit_globals_after_BEGIN {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program

    if (0) {
        my $ast = dump_to_AST_after_BEGIN(@_);
        print STDERR Data::Dumper::Dumper($ast);
    }

    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";

    for my $name (sort keys %$scope) {
        my $sigil = substr($name, 0, 1);
        my $item = $scope->{$name};
        if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
            # TODO
            # _dump_global($item, $seen, $dumper_seen, $vars, $tab);
            push @$vars, "# don't know how to initialize subroutine $name";
            next;
        }

        # TODO - emit lexicals
        next
            if substr($name, 1, 2) eq "C_";

        if (substr($name, 7, 1) lt 'A') {
            # encode special variable names like $main::" to ${'main::"'}
            $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
        }
        my $ast = $item->{ast};
        if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
            # "our" variables are lexical aliases; we want the original global variable name
            $name =
                  ($ast->{_real_sigil} || $ast->{sigil})
                . ($ast->{namespace} || $ast->{_namespace} || "C_")
                . "::" . $ast->{name};
        }
        if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
            my $value = eval($name);
            my $dump = _dumper( $value, "  ", $dumper_seen, $name );
            next if $dump eq 'undef';
            push @$vars, "$name = " . $dump . ";";
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && ($sigil eq '@' || $sigil eq '%')) {
            my $value = eval("\\" . $name);
            my $dump = _dumper( $value, "  ", $dumper_seen, '\\' . $name );
            my $bareword = substr($name, 1);
            push @$vars, "*$bareword = " . $dump . ";";
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
            # *mysub = sub {...}
            my $bareword = substr($name, 1);

            if (exists &{$bareword}) {
                my $sub = \&{$bareword};
                my $dump = _dumper($sub, '  ', $dumper_seen, '\\&' . $bareword);
                push @$vars, "*$bareword = " . $dump . ';';
            }
            if (defined ${$bareword}) {
                my $sub = \${$bareword};
                my $dump = _dumper($sub, '  ', $dumper_seen, '\\$' . $bareword);
                push @$vars, "*$bareword = " . $dump . ';';
            }
            if (@{$bareword}) {
                my $sub = \@{$bareword};
                my $dump = _dumper($sub, '  ', $dumper_seen, '\\@' . $bareword);
                push @$vars, "*$bareword = " . $dump . ';';
            }
            if (keys %{$bareword}) {
                my $sub = \%{$bareword};
                my $dump = _dumper($sub, '  ', $dumper_seen, '\\%' . $bareword);
                push @$vars, "*$bareword = " . $dump . ';';
            }

        }
        else {
            push @$vars, "# don't know how to initialize variable $name";
        }
    }
    return join("\n", @$vars);
}

1;


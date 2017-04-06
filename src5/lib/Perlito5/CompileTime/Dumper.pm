package Perlito5::CompileTime::Dumper;

use Perlito5::DumpToAST;
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

sub _dump_AST_from_scope {
    my ($name, $item, $vars, $dumper_seen,) = @_;
    @_ = ();    # don't dump @_

    my $sigil = substr($name, 0, 1);
    if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
        # TODO
        # _dump_global($item, $seen, $dumper_seen, $vars, $tab);
        warn "# don't know how to initialize subroutine $name in BEGIN";
        return;
    }

    # TODO - emit lexicals

    if (substr($name, 7, 1) lt 'A') {
        # encode special variable names like $main::" to ${'main::"'}
        $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
    }
    my $ast = $item->{ast};
    if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
        # "our" variables are lexical aliases; we want the original global variable name
        $name =
              ($ast->{_real_sigil} || $ast->{sigil})
            . ($ast->{namespace} || $ast->{_namespace})
            . "::" . $ast->{name};
    }
    my $bareword = substr($name, 1);
    if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
        my $value = ${$bareword};
        return if !defined($value);
        push @$vars, # "$name = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        $ast,
                        Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #'$name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '@') {
        my $value = \@{$bareword};
        push @$vars, # "*$bareword = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                        Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #''\\' . $name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '%') {
        my $value = \%{$bareword};
        push @$vars, # "*$bareword = ", $dump;
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                        Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #''\\' . $name ),
                    ],
                );
    }
    elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
        # *mysub = sub {...}
        if (exists &{$bareword}) {
            my $value = \&{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #''\\&' . $bareword ),
                        ],
                    );
        }
        if (defined ${$bareword}) {
            my $value = \${$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #''\\$' . $bareword ),
                        ],
                    );
        }
        if (@{$bareword}) {
            my $value = \@{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #''\\@' . $bareword ),
                        ],
                    );
        }
        if (keys %{$bareword}) {
            my $value = \%{$bareword};
            push @$vars, # "*$bareword = ", $dump;
                    Perlito5::AST::Apply->new(
                        code => 'infix:<=>',
                        arguments => [
                            Perlito5::AST::Var->new( %$ast, sigil => '*' ),
                            Perlito5::DumpToAST::dump_to_ast( $value, $dumper_seen, $ast ),  #'\\%' . $bareword ),
                        ],
                    );
        }

    }
    else {
        warn "# don't know how to initialize variable $name in BEGIN";
    }

}

sub push_AST_refs {
    my ($vars, $array, $value) = @_;

    if (ref($value) eq 'Perlito5::AST::Apply' && $value->{code} eq 'circumfix:<[ ]>') {
        push @$vars, Perlito5::AST::Apply::PUSH(
            $array,
            Perlito5::AST::Apply->new( %$value, arguments => [] ),
        );
        my $deref = Perlito5::AST::Index::INDEX($array, Perlito5::AST::Int->new(int => -1));
        for my $arg (@{$value->{arguments}}) {
            push_AST_refs($vars, $deref, $arg);
        }
        return;
    }

    if (ref($value) eq 'Perlito5::AST::Apply' && $value->{code} eq 'circumfix:<{ }>') {
        push @$vars, Perlito5::AST::Apply::PUSH(
            $array,
            Perlito5::AST::Apply->new( %$value, arguments => [] ),
        );
        for my $arg (@{$value->{arguments}}) {
            my $hash = Perlito5::AST::Lookup::LOOKUP($array, $arg->{arguments}[0]);
            push @$vars,
                Perlito5::AST::Apply->new(
                    code => 'infix:<=>',
                    arguments => [
                        $hash,
                        $arg->{arguments}[1],
                    ],
                );
        }
        return;
    }

    push @$vars, Perlito5::AST::Apply::PUSH(
        $array,
        $value,
    );
}

sub dump_to_AST_after_BEGIN {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program
    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $dumper_seen = {};

    for my $name (sort keys %$scope) {
        my $item = $scope->{$name};
        _dump_AST_from_scope($name, $item, $vars, $dumper_seen);
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
        for my $var_id (sort keys %$captures) {
            next if $var_id eq "__PKG__";
            if ($var_id eq '__SUB__') {
                my $sub_id = $captures->{$var_id};
                $ast = $Perlito5::BEGIN_SUBS{$sub_id};

                $sub_name = $ast->{namespace} . "::" . $ast->{name}
                    if $ast->{name};;

                my @data = $ast->emit_perl5();
                my $out = [];
                Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                $source = join( '', @$out ) . ";";
            }
            else {
                my $var_ast = $Perlito5::BEGIN_LEXICALS{$var_id};
                my $sigil = ($var_ast->{_real_sigil} || $var_ast->{sigil});
                if ($var_ast->{_decl} eq "our") {
                    push @vars, 
                        'our ' . $sigil . $var_ast->{name} . '; ';
                }
                else {
                    push @vars, 
                        'my ' . $sigil . $var_ast->{name} . ' = ' . _dumper_deref($captures->{$var_id}, $tab1, $seen, $pos) . '; ';
                }
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

    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";

    # exclude %ENV, $] - these should use whatever is set at runtime
    delete $scope->{'%main::ENV'};
    delete $scope->{'$main::]'};

    for my $v ( '$main::0', '$main::a', '$main::b', '$main::_' ) {
        # inject special variables like $0 (script name) in the scope, if it is not there already
        my ($sigil, $namespace, $name) = $v =~ /^([$@%])(\w+)::(.*)$/;
        $scope->{$v} //= {
            'ast' => Perlito5::AST::Var->new(
                'name'      => $name,
                'sigil'     => $sigil,
                '_decl'     => 'global',
                'namespace' => $namespace,
            ),
        };
    }

    # dump __END__ blocks
    $scope->{'@Perlito5::END_BLOCK'} //= {
        'ast' => Perlito5::AST::Var->new(
            'namespace' => 'Perlito5',
            'name'      => 'END_BLOCK',
            'sigil'     => '@',
            '_decl'     => 'global',
        ),
        value => \@Perlito5::END_BLOCK,
    };

    # dump __INIT__ blocks
    $scope->{'@Perlito5::INIT_BLOCK'} //= {
        'ast' => Perlito5::AST::Var->new(
            'namespace' => 'Perlito5',
            'name'      => 'INIT_BLOCK',
            'sigil'     => '@',
            '_decl'     => 'global',
        ),
        value => \@Perlito5::INIT_BLOCK,
    };

    # dump __DATA__ contents
    $scope->{'%Perlito5::DATA_SECTION'} //= {
        'ast' => Perlito5::AST::Var->new(
            'namespace' => 'Perlito5',
            'name'      => 'DATA_SECTION',
            'sigil'     => '%',
            '_decl'     => 'global',
        ),
        value => \%Perlito5::DATA_SECTION,
    };

    for my $id (keys %Perlito5::BEGIN_SCRATCHPAD) {
        # BEGIN side-effects
        my $ast = $Perlito5::BEGIN_SCRATCHPAD{$id};
        my $sigil = $ast->{_real_sigil} || $ast->{sigil};
        if (!$ast->{namespace}) {
            $ast->{namespace} = "Perlito5::BEGIN";
            $ast->{name} = "_" . $id . "_" . $ast->{name};
        }
        my $fullname = "$ast->{namespace}::$ast->{name}";

        # print STDERR "BEGIN SIDE EFECT: $sigil $fullname\n";

        if ($sigil eq '$') {
            $scope->{$sigil . $fullname} //= {
                ast   => $ast,
                value => \${$fullname},
            };
        }
        elsif ($sigil eq '@') {
            $scope->{$sigil . $fullname} //= {
                ast   => $ast,
                value => \@{$fullname},
            };
        }
        elsif ($sigil eq '%') {
            $scope->{$sigil . $fullname} //= {
                ast   => $ast,
                value => \%{$fullname},
            };
        }
    }

    if (0) {
        my $ast = dump_to_AST_after_BEGIN($scope);
        print STDERR Data::Dumper::Dumper($ast);

        my @data = map { $_->emit_perl5 } @$ast;
        my $out = [];
        Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
        my $source_new = join( '', @$out ), ";1\n";
        print STDERR "[[[ $source_new ]]]\n";
    }


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

        if (substr($name, 7, 1) lt 'A') {
            # encode special variable names like $main::" to ${'main::"'}
            $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
        }
        my $ast = $item->{ast};
        if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
            # "our" variables are lexical aliases; we want the original global variable name
            $name =
                  ($ast->{_real_sigil} || $ast->{sigil})
                . ($ast->{namespace} || $ast->{_namespace})
                . "::" . $ast->{name};
            next if $scope->{$name};    # skip if we've seen this before
        }
        next if $name eq '@main::ARGV';
        my $bareword = substr($name, 1);
        if (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '$') {
            my $value;
            if ($name eq '$main::`') {
                $value = $`;    # perl doesn't like $main::`
            }
            else {
                $value = ${$bareword};
            }
            my $dump = _dumper( $value, "  ", $dumper_seen, $name );
            next if $dump eq 'undef';
            push @$vars, "$name = " . $dump . ";";
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '%') {
            my $value = \%{$bareword};
            my $dump = _dumper( $value, "  ", $dumper_seen, '\\' . $name );
            push @$vars, "*$bareword = " . $dump . ";";
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '@') {
            my $value = \@{$bareword};
            my $dump = _dumper( $value, "  ", $dumper_seen, '\\' . $name );
            push @$vars, "*$bareword = " . $dump . ";";
        }
        elsif (ref($ast) eq 'Perlito5::AST::Var' && $sigil eq '*') {
            # *mysub = sub {...}

            # *{'strict::import'}   ???
            $bareword = substr($bareword, 2, -2) if substr($bareword,0,2) eq "{'";

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


package Perlito5::CompileTime::Dumper;

use Perlito5::DumpToAST;
use strict;

# TODO
# sub generate_eval_string {
#     my ($source, $strict) = @_;
#     my $source_new = "";
#     $@ = "";
#     eval {
#         # print STDERR "[[[ $source ]]]\n";
#         my $m = Perlito5::Grammar::exp_stmts($source, 0);
#         my $block = Perlito5::AST::Block->new( stmts => Perlito5::Match::flat($m) );
# 
#         ## TODO: enable emit_compile_time()
#         ##
#         ##  $ node perlito5.js -I src5/lib  t5/op/pow.t
#         ##  [[[ { package main;
#         ##  {
#         ##      chdir 't' if -d 't';
#         ##      @INC = '../lib';
#         ##      require './test.pl';
#         ##  } }; 1 ]]]
#         ##  Error in BEGIN block: TypeError: Cannot call method 'p5aget' of undefined
#         ##
#         # my @data = $block->emit_compile_time;
# 
#         my @data = $block->emit_perl5;
#         my $out = [];
#         Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
#         $source_new = join( '', @$out ), ";1\n";
#         # print STDERR "[[[ $source_new ]]]\n";
#     };
#     return $source_new;
# }

sub _dump_AST_from_scope {
    my ($name, $item, $vars, $dumper_seen,) = @_;
    no strict 'refs';

    my $sigil = substr($name, 0, 1);
    if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
        # TODO
        # _dump_global($item, $seen, $dumper_seen, $vars, $tab);
        # warn "# don't know how to initialize subroutine $name in BEGIN";
        return;
    }

    if (substr($name, 7, 1) lt 'A') {
        # encode special variable names like $main::" to ${'main::"'}
        $name = $sigil . '{' . Perlito5::Dumper::escape_string(substr($name,1)) . '}'
    }
    my $ast = $item->{ast};
    if (ref($ast) eq 'Perlito5::AST::Var' && $ast->{_decl} eq "our") {
        # "our" variables are lexical aliases; we want the original global variable name
        $ast = Perlito5::AST::Var->new(
            %$ast,
            sigil => $ast->{'_real_sigil'} || $ast->{'sigil'},
            namespace => $ast->{'namespace'} || $ast->{'_namespace'},
            decl => 'global',
        );
        $name = $ast->{sigil} . $ast->{namespace} . "::" . $ast->{name};
        # return if $Perlito5::GLOBAL->{$name};    # skip if we've seen this before
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

        # @{'X::ISA'}   ???
        $bareword = substr($bareword, 2, -2) if substr($bareword,0,2) eq "{'";

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

        # *{'strict::import'}   ???
        $bareword = substr($bareword, 2, -2) if substr($bareword,0,2) eq "{'";

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
        # warn "# don't know how to initialize variable $name in BEGIN";
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

    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";

    # exclude %ENV, $] - these should use whatever is set at runtime
    delete $scope->{'%main::ENV'};
    delete $scope->{'$main::]'};
    delete $scope->{'$main::ARGV'};
    delete $scope->{'@main::_'};
    local $_;   # not sure about keeping $_

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

    # dump @ISA
    for my $pkg (keys %{$Perlito5::PACKAGES}) {;
        no strict 'refs';
        if (@{ $pkg . "::ISA" }) {
            $scope->{ '@' . $pkg . "::ISA" } //= {
                'ast' => Perlito5::AST::Var->new(
                    'name'      => "ISA",
                    'sigil'     => '@',
                    '_decl'     => 'global',
                    'namespace' => $pkg,
                ),
                value => \@{ $pkg . "::ISA" },
            };
        }
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

    # return a structure with the global variable declarations
    my $vars = [];
    my $dumper_seen = {};

    # dump subroutine stubs (prototypes)
    for my $fullname (sort keys %$Perlito5::PROTO) {
        my $proto = $Perlito5::PROTO->{$fullname};
        my @parts = split "::", $fullname;
        my $name = pop @parts;
        push @$vars,
          Perlito5::AST::Sub->new(
            'namespace'  => join( "::", @parts ),
            'sig'        => $proto,
            'name'       => $name,
            'block'      => undef,
            'attributes' => []
          );
    }

    for my $name (sort keys %$scope) {
        my $item = $scope->{$name};
        _dump_AST_from_scope($name, $item, $vars, $dumper_seen);
    }
    return $vars;
}

1;


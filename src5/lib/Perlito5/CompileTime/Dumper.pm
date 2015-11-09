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
        for my $var (keys %$captures) {
            push @vars, 
                'my ' . $var . ' = ' . _dumper($captures->{$var}, $tab1, $seen, $pos) . '; ';
        }
        return join('',
            'do { ',
                @vars,
                'sub { "DUMMY" } ',
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

sub _emit_globals {
    my ($scope, $seen, $dumper_seen, $vars, $tab) = @_;
    my $block = $scope->{block};
    for my $item (@$block) {
        if (ref($item) eq 'Perlito5::AST::Var' && !$item->{_decl}) {
            $item->{_decl} = 'global';
        }
        if (ref($item) eq 'Perlito5::AST::Var' && $item->{_decl} eq 'global') {
            $item->{namespace} ||= $item->{_namespace};
            next if $item->{name} eq '0' || $item->{name} > 0;  # skip regex and $0
            _dump_global($item, $seen, $dumper_seen, $vars, $tab);
        }
        if (ref($item) eq 'Perlito5::AST::Var' && $item->{_decl} eq 'my') {
            my $id = $item->{_id};
            if (!$seen->{$id}) {
                push @$vars, $tab . emit_compiletime_lexical($item) . "  # my " . $item->{sigil} . $item->{name} . "\n";
            }
            $seen->{$id} = 1;
        }
        if ( ref($item) eq 'HASH' && $item->{block} ) {
            # lookup in the inner scope
            push @$vars, $tab . "{\n";
            _emit_globals($item, $seen, $dumper_seen, $vars, $tab . "  ");
            push @$vars, $tab . "}\n";
        }
    }
}

sub emit_compiletime_lexical {
    my $item = shift;
    # the internal compile-time namespace is "C_"
    return $item->{sigil} . 'C_::' . $item->{name} . "_" . $item->{_id};
}

sub emit_globals_scope {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program
    my $scope = shift() // $Perlito5::BASE_SCOPE;
    my @vars;
    my %seen;
    my $dumper_seen = {};
    my $tab = "";
    _emit_globals($scope, \%seen, $dumper_seen, \@vars, $tab);
    return join("", @vars);
}

sub emit_globals {
    # return a structure with the global variable declarations
    # this is used to initialize the ahead-of-time program
    my $scope = shift() // $Perlito5::GLOBAL;
    my $vars = [];
    my $seen = {};
    my $dumper_seen = {};
    my $tab = "";

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

    # example of how to enable this dump:
    #   $ PERLITO5DEV=1 perl perlito5.pl -Isrc5/lib -I. -It -C_globals -e ' use X; xxx(); sub xyz { 123 } my $z; BEGIN { $a = 3; $z = 3 } '

    for my $name (keys %$scope) {
        my $item = $scope->{$name};
        if (ref($item) eq 'Perlito5::AST::Sub' && $item->{name}) {
            _dump_global($item, $seen, $dumper_seen, $vars, $tab);
        }
        else {
            $item->{value} = eval($name);
            push @$vars, "$name = " . _dumper( $item, "  ", $dumper_seen, $name . "->{value}" ) . ";\n";
        }
    }
    return join("", @$vars);
}

1;


class Main {
    use MiniPerl6::Go::Emitter;
    use MiniPerl6::Lisp::Emitter;
    use MiniPerl6::Perl5::Emitter;
    use MiniPerl6::Javascript::Emitter;
    use MiniPerl6::Parrot::Emitter;
    use MiniPerl6::Python::Emitter;
    use MiniPerl6::Ruby::Emitter;
    use MiniPerl6::Grammar;
    use MiniPerl6::Grammar::Control;
    use MiniPerl6::Grammar::Mapping;
    use MiniPerl6::Grammar::Regex;
    use MiniPerl6::Emitter::Token;

    my $_V6_COMPILER_NAME    = 'MiniPerl6';
    my $_V6_COMPILER_VERSION = '6.0';
    my $source      = '';
    my $backend     = '';
    my $execute     = 0;
    my $verbose     = 0;
    my $comp_units  = [];
    my $perl6lib    = './lib';
    my $expand_use  = 1;

    if $verbose {
        warn "// MiniPerl6 compiler";
        warn "// ARGS: ", @*ARGS.perl;
    }

    my %module_seen;

token module_name {
    <MiniPerl6::Grammar.ident>
    [   '::' <module_name>  { make [ ~$<MiniPerl6::Grammar.ident>, @( $$<module_name> ) ] }
    |   ''                  { make [ ~$<MiniPerl6::Grammar.ident> ] }
    ]
}
sub modulename_to_filename ($s) {
    my $ident = Main.module_name( $s, 0 );
    return ($$ident).join("/");
}
sub add_comp_unit (@parse) {
    for @parse -> $comp_unit {
        if $comp_unit.isa('CompUnit') {
            if $verbose {
                warn "parsed comp_unit: '", $comp_unit.name, "'"; 
            }
            for @( $comp_unit.body ) -> $stmt {
                if $expand_use && $stmt.isa('Use') {
                    my $module_name = $stmt.mod;
                    if !(%module_seen{$module_name}) {
                        %module_seen{$module_name} = 1;
                        # say "  now use: ", $module_name;
                        if ($backend eq 'perl5') || ($backend eq 'ast-perl6') {
                            # skip 'use' statements for this backend
                        }
                        else {
                            # TODO - look for a precompiled version
                            # build the filename
                            my $filename = $module_name;
                            $filename = $perl6lib ~ '/' ~ modulename_to_filename($filename) ~ '.pm';
                            if ( $verbose ) {
                                warn "// now loading: ", $filename;
                            }
                            # load source 
                            my $source = IO::slurp( $filename );

                            # compile; push AST into comp_units
                            # warn $source;
                            my $m = MiniPerl6::Grammar.exp_stmts($source, 0);
                            add_comp_unit($$m);
                        }
                    }
                }
            }
        }
        $comp_units.push( $comp_unit );
        # say "comp_unit done";
    }
}

    if (@*ARGS[0] eq '-v') || (@*ARGS[0] eq '--verbose') {
        $verbose = 1;
        shift @*ARGS;
    }
    if substr(@*ARGS[0], 0, 2) eq '-C' {
        $backend = substr(@*ARGS[0], 2, 10);
        $execute = 0;
        shift @*ARGS;
        if ($backend eq 'perl5') || ($backend eq 'python') || ($backend eq 'ruby') {
            $expand_use = 0;
        }
    }
    if substr(@*ARGS[0], 0, 2) eq '-B' {
        $backend = substr(@*ARGS[0], 2, 10);
        $execute = 1;
        shift @*ARGS;
        if ($backend eq 'perl5') || ($backend eq 'python') || ($backend eq 'ruby') {
            $expand_use = 0;
        }
    }
    if (@*ARGS[0] eq '-V') || (@*ARGS[0] eq '--version') {
        $backend = '';
        say $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
        shift @*ARGS;
    }
    elsif (@*ARGS[0] eq '-h') || (@*ARGS[0] eq '--help') || ($backend eq '') {
        $backend = '';
        say $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION, "
mp6 [switches] [programfile]
  switches:
    -h --help
    -v --verbose
    -V --version
    -Ctarget        target backend: go, js, lisp, parrot, perl5, python, ruby, ast-perl6
    --expand_use --noexpand_use
                    expand 'use' statements at compile time
    -e program      one line of program (omit programfile)
";
        shift @*ARGS;
    }
    if @*ARGS[0] eq '--expand_use' {
        $expand_use = 1;
        shift @*ARGS;
    }
    if @*ARGS[0] eq '--noexpand_use' {
        $expand_use = 0;
        shift @*ARGS;
    }
    if $backend && @*ARGS {
        my $prelude_filename;
        if $backend eq 'lisp' {
            $prelude_filename = $perl6lib ~ '/MiniPerl6/Lisp/Prelude.pm';
        }
        if $backend eq 'js' {
            $prelude_filename = $perl6lib ~ '/MiniPerl6/Javascript/Prelude.pm';
        }
        if $backend eq 'go' {
            $prelude_filename = $perl6lib ~ '/MiniPerl6/Go/Prelude.pm';
        }
        if $prelude_filename {
            if $verbose {
                warn "// loading lib: ", $prelude_filename;
            }
            $source = IO::slurp( $prelude_filename );
            my $m = MiniPerl6::Grammar.exp_stmts($source, 0);
            add_comp_unit($$m);
        }
        if @*ARGS[0] eq '-e' {
            shift @*ARGS;
            if $verbose {
                warn "// source from command line: ", @*ARGS[0];
            }
            $source = @*ARGS.shift;
        }
        else {
            if $verbose {
                warn "// source from file: ", @*ARGS[0];
            }
            $source = IO::slurp( @*ARGS.shift );
        }

        if $verbose {
            warn "// backend: ", $backend;
            warn "now parsing";
        }

        my $m = MiniPerl6::Grammar.exp_stmts($source, 0);
        add_comp_unit($$m);

        if $backend eq 'ast-perl6' {
            say "# AST dump - do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say $comp_units.perl;
        }
        if $backend eq 'go' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/MiniPerl6/Go/Runtime.go';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            say CompUnit::emit_go_program( $comp_units );
        }
        if $backend eq 'lisp' {
            say ";; Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/MiniPerl6/Lisp/Runtime.lisp';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            say CompUnit::emit_lisp_program( $comp_units );
            say '(compiler-main)';
            say ';; Note: the line below creates a binary executable:';
            say ';; (sb-ext:save-lisp-and-die "tmp.out" :toplevel \'compiler-main :executable t )';
        }
        if $backend eq 'perl5' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say "use v5;";
            say "use utf8;";
            say "use strict;";
            say "use warnings;";
            say "no warnings ('redefine', 'once', 'void', 'uninitialized', 'misc', 'recursion');";
            say "use MiniPerl6::Perl5::Runtime;";
            say 'our $MATCH = MiniPerl6::Match->new();';
            print CompUnit::emit_perl5_program( $comp_units );
            say "1;";
        }
        if $backend eq 'js' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/MiniPerl6/Javascript/Runtime.js';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            for @($comp_units) -> $c {
                say $c.emit_javascript;
            }
        }
        if $backend eq 'python' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say "";
            say "from miniperl6.python.runtime import *";
            say "from MiniPerl6__Python__Prelude import *";
            say "import __builtin__";
            say "__all__ = []";
            say "";
            for @($comp_units) -> $c {
                say $c.emit_python;
            }
        }
        if $backend eq 'ruby' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say "";
            say "require 'MiniPerl6/Ruby/Runtime.rb'";
            say "";
            for @($comp_units) -> $c {
                say $c.emit_ruby;
            }
        }
        if $backend eq 'parrot' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            for @($comp_units) -> $c {
                say $c.emit_parrot;
            }
        }
    }
}


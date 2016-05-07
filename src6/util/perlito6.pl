use v6;

class Main {
    use Perlito6::Emitter::Token;
    use Perlito6::Expression;
    use Perlito6::Go::Emitter;
    use Perlito6::Grammar::Control;
    use Perlito6::Grammar::Regex;
    use Perlito6::Grammar;
    use Perlito6::Java::Emitter;
    use Perlito6::JavaScript::Emitter;
    use Perlito6::Lisp::Emitter;
    use Perlito6::Macro;
    use Perlito6::Parrot::Emitter;
    use Perlito6::Perl5::Emitter;
    use Perlito6::Precedence;
    use Perlito6::Python::Emitter;
    use Perlito6::Ruby::Emitter;
    use Perlito6::Runtime;

    my $_V6_COMPILER_NAME    = 'Perlito6';
    my $_V6_COMPILER_VERSION = '9.0';
    my $source      = '';
    my $backend     = '';
    my $execute     = 0;
    my $verbose     = 0;
    my $comp_units  = [];
    my $perl6lib    = './src6/lib';
    my $expand_use  = 1;

    if $verbose {
        warn "// Perlito Perl6 compiler";
        warn "// ARGS: ", @*ARGS.perl;
    }

    my %module_seen;

token module_name {
    <Perlito6::Grammar.ident>
    [   '::' <module_name>  { make [ ~$<Perlito6::Grammar.ident>, @( $$<module_name> ) ] }
    |   ''                  { make [ ~$<Perlito6::Grammar.ident> ] }
    ]
}
sub modulename_to_filename ($s) {
    my $ident = Main.module_name( $s, 0 );
    return ($$ident).join("/");
}

sub expand_use($stmt) {
    my $module_name = $stmt.mod;
    return 
        if $module_name eq 'v6';
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
            my $m = Perlito6::Grammar.exp_stmts($source, 0);
            add_comp_unit($$m);
        }
    }
}

sub add_comp_unit (@parse) {
    for @parse -> $comp_unit {
        if $expand_use && $comp_unit.isa('Use') {
            expand_use($comp_unit);
        }
        elsif $comp_unit.isa('CompUnit') {
            if $verbose {
                warn "parsed comp_unit: '", $comp_unit.name, "'"; 
            }
            for @( $comp_unit.body ) -> $stmt {
                if $expand_use && $stmt.isa('Use') {
                    expand_use($stmt);
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
perlito [switches] [programfile]
  switches:
    -h --help
    -v --verbose
    -V --version
    -Ctarget        target backend: go, js, lisp, parrot, perl5, python, ruby, ast-perl6, java
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
            $prelude_filename = $perl6lib ~ '/Perlito6/Lisp/Prelude.pm';
        }
        if $backend eq 'js' {
            $prelude_filename = $perl6lib ~ '/Perlito6/JavaScript/Prelude.pm';
        }
        if $backend eq 'go' {
            $prelude_filename = $perl6lib ~ '/Perlito6/Go/Prelude.pm';
        }
        if $prelude_filename {
            if $verbose {
                warn "// loading lib: ", $prelude_filename;
            }
            $source = IO::slurp( $prelude_filename );
            my $m = Perlito6::Grammar.exp_stmts($source, 0);
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

        my $m = Perlito6::Grammar.exp_stmts($source, 0);
        add_comp_unit($$m);

        $comp_units = [
                CompUnit.new(
                    name => 'GLOBAL',
                    body => $comp_units,
                ),
            ];

        if $backend eq 'ast-perl6' {
            say "# AST dump - do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say $comp_units.perl;
        }
        if $backend eq 'go' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/Perlito6/Go/Runtime.go';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            say CompUnit::emit_go_program( $comp_units );
        }
        if $backend eq 'lisp' {
            say ";; Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/Perlito6/Lisp/Runtime.lisp';
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
            print CompUnit::emit_perl5_program( $comp_units );
        }
        if $backend eq 'js' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib ~ '/Perlito6/JavaScript/Runtime.js';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            print CompUnit::emit_javascript_program( $comp_units );
        }
        if $backend eq 'python' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say "#-*- coding: utf-8 -*-";
            say "";
            say "from Perlito6__Python__Runtime import *";
            say "from Perlito6__Python__Prelude import *";
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
            say "require 'Perlito6/Ruby/Runtime.rb'";
            say "require 'Perlito6__Ruby__Prelude.rb'";
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
        if $backend eq 'java' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            print CompUnit::emit_java_program( $comp_units );
        }
    }
}

=begin

=head1 NAME

util/perlito6.pl - Perl6 compiler

=head1 SYNOPSIS

    perlito6 -Cjs program.pl

=head1 DESCRIPTION

This program reads Perl6 source code and generates native code.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


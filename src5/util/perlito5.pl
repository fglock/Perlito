use v5;

package Main;
use Perlito5::Emitter::Token;
use Perlito5::Expression;
use Perlito5::Grammar::Control;
use Perlito5::Grammar::Regex;
use Perlito5::Grammar;
use Perlito5::Javascript::Emitter;
use Perlito5::Macro;
use Perlito5::Perl5::Emitter;
use Perlito5::Precedence;
## use Perlito5::Python::Emitter;
## use Perlito5::Ruby::Emitter;
use Perlito5::Runtime;

my $_V6_COMPILER_NAME    = 'Perlito5';
my $_V6_COMPILER_VERSION = '8.0';
my $source      = '';
my $backend     = '';
my $execute     = 0;
my $verbose     = 0;
my $comp_units  = [];
my $perl6lib    = './src5/lib';
my $expand_use  = 1;

if $verbose {
    warn "// Perlito5 compiler";
    warn "// ARGS: ", @*ARGS->perl;
}

my %module_seen;

token module_name {
    <Perlito5::Grammar.ident>
    [   '::' <module_name>  { make [ '' . $<Perlito5::Grammar.ident>, @( $$<module_name> ) ] }
    |   ''                  { make [ '' . $<Perlito5::Grammar.ident> ] }
    ]
}
sub modulename_to_filename {
    my $s = shift;
    my $ident = Main->module_name( $s, 0 );
    return ($$ident)->join("/");
}

sub expand_use {
    my $stmt = shift;
    my $module_name = $stmt->mod;
    return
        if $module_name eq 'v5'
        || $module_name eq 'strict'
        || $module_name eq 'feature';
    if !(%module_seen{$module_name}) {
        %module_seen{$module_name} = 1;
        # say "  now use: ", $module_name;
        if ($backend eq 'perl5') || ($backend eq 'ast-perl5') {
            # skip 'use' statements for this backend
        }
        else {
            # TODO - look for a precompiled version
            # build the filename
            my $filename = $module_name;
            $filename = $perl6lib . '/' . modulename_to_filename($filename) . '.pm';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            # load source
            my $source = IO::slurp( $filename );

            # compile; push AST into comp_units
            # warn $source;
            my $m = Perlito5::Grammar->exp_stmts($source, 0);
            add_comp_unit($$m);
        }
    }
}

sub add_comp_unit {
    my $parse = shift;
    for my $comp_unit (@$parse) {
        if $expand_use && $comp_unit->isa('Use') {
            expand_use($comp_unit);
        }
        elsif $comp_unit->isa('CompUnit') {
            if $verbose {
                warn "parsed comp_unit: '", $comp_unit->name, "'";
            }
            for my $stmt (@( $comp_unit->body )) {
                if $expand_use && $stmt->isa('Use') {
                    expand_use($stmt);
                }
            }
        }
        $comp_units->push( $comp_unit );
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
perlito5 [switches] [programfile]
  switches:
    -h --help
    -v --verbose
    -V --version
    -Ctarget        target backend: js, perl5, ast-perl5
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
        if $backend eq 'js' {
            $prelude_filename = $perl6lib . '/Perlito5/Javascript/Prelude.pm';
        }
        if $prelude_filename {
            if $verbose {
                warn "// loading lib: ", $prelude_filename;
            }
            $source = IO::slurp( $prelude_filename );
            my $m = Perlito5::Grammar->exp_stmts($source, 0);
            add_comp_unit($$m);
        }
        if @*ARGS[0] eq '-e' {
            shift @*ARGS;
            if $verbose {
                warn "// source from command line: ", @*ARGS[0];
            }
            $source = @*ARGS->shift;
        }
        else {
            if $verbose {
                warn "// source from file: ", @*ARGS[0];
            }
            $source = IO::slurp( @*ARGS->shift );
        }

        if $verbose {
            warn "// backend: ", $backend;
            warn "now parsing";
        }

        my $m = Perlito5::Grammar->exp_stmts($source, 0);
        add_comp_unit($$m);

        $comp_units = [
                CompUnit->new(
                    name => 'GLOBAL',
                    body => $comp_units,
                ),
            ];

        if $backend eq 'ast-perl5' {
            say "# AST dump - do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            say $comp_units->perl;
        }
        if $backend eq 'perl5' {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            print CompUnit::emit_perl5_program( $comp_units );
        }
        if $backend eq 'js' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            my $filename = $perl6lib . '/Perlito5/Javascript/Runtime.js';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            my $source = IO::slurp( $filename );
            say $source;
            print CompUnit::emit_javascript_program( $comp_units );
        }
        if $backend eq 'java' {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            print CompUnit::emit_java_program( $comp_units );
        }
    }

=begin

=head1 NAME

util/perlito5.pl - Perl5 compiler

=head1 SYNOPSIS

    perlito5 -Cjs program.pl

=head1 DESCRIPTION

This program reads Perl6 source code and generates native code.

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

L<http://www.perl.org>

=head1 COPYRIGHT

Copyright 2011 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=end


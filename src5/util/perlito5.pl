#! /usr/bin/perl

use v5;

package Perlito5;
use feature 'say';
use Perlito5;
use Perlito5::Compiler;
use Perlito5::CompileTime::Emitter;
use Perlito5::CompileTime::Dumper;
use Perlito5::Grammar::Regex6;
use Perlito5::Emitter::Token;
use Perlito5::Dumper;
use Perlito5::JSON;

use Perlito5::Javascript2::Emitter;
use Perlito5::Javascript2::Runtime;
use Perlito5::Javascript2::Array;
use Perlito5::Javascript2::CORE;
use Perlito5::Javascript2::IO;
use Perlito5::Javascript2::Sprintf;

use Perlito5::Javascript3::Emitter;
use Perlito5::Javascript3::Runtime;
use Perlito5::Javascript3::CORE;
use Perlito5::Javascript3::IO;
use Perlito5::Javascript3::Sprintf;

use Perlito5::Perl5::Emitter;
use Perlito5::Perl5::PrettyPrinter;
use Perlito5::Perl5::Runtime;

use Perlito5::Perl6::Emitter;
use Perlito5::Perl6::PrettyPrinter;

use Perlito5::XS::Emitter;      # experimental

use Perlito5::Java::Emitter;  # experimental
use Perlito5::Java::Runtime;

## use Perlito5::Python::Emitter;
## use Perlito5::Ruby::Emitter;

use strict;

my $_V5_COMPILER_NAME    = 'Perlito5';
my $_V5_COMPILER_VERSION = $Perlito5::VERSION;
my $source      = '';
my $backend     = $^O;
my $execute     = 1;
my $verbose     = 0;
my $expand_use  = 1;
my $boilerplate = 1;
my $bootstrapping = 0;
my $wrapper_begin = '';
my $wrapper_end = '';
my $wrapper_priority = 0;
my @Use;

if ($verbose) {
    warn "// Perlito5 compiler";
    warn "// ARGV: @ARGV";
}

my $help_message = "
perlito5 [switches] [programfile]
  switches:
    -e program      one line of program (omit programfile)
    -h --help
    -Idirectory     specify \@INC/include directory (several -I's allowed)
    -[mM][-]module  execute \"use/no module...\" before executing program
    -n              assume \"while (<>) { ... }\" loop around program
    -p              assume loop like -n but print line also, like sed
    -V --version
    -v
    --verbose
    -Ctarget        target backend: js, perl5, perl6, xs, java
    -Cast-perl5     emits a dump of the abstract syntax tree as a Perl dump
    -Cast-json      emits a dump of the abstract syntax tree in JSON format
    --expand_use --noexpand_use
                    expand 'use' statements at compile time
    --boilerplate --noboilerplate
                    emits or not boilerplate code
    --bootstrapping set this when compiling the compiler,
                    otherwise the new subroutine definitions will overwrite the current compiler
";
my $copyright_message = <<"EOT";
This is Perlito5 $_V5_COMPILER_VERSION, an implementation of the Perl language.

The Perl language is Copyright 1987-2012, Larry Wall
The Perlito5 implementation is Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

Perl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.

Complete documentation for Perl, including FAQ lists, should be found on
this system using "man perl" or "perldoc perl".  If you have access to the
Internet, point your browser at http://www.perl.org/, the Perl Home Page.
EOT

sub chomp_switch {
    # split switches like "-pie" into "-p -i -e"
    my $s = substr($ARGV[0], 2);
    if ($s) {
        $ARGV[0] = "-$s";
    }
    else {
        shift @ARGV;
    }
}

push @Use, "no warnings";
push @Use, "no strict";

while (substr($ARGV[0], 0, 1) eq '-'
    && substr($ARGV[0], 0, 2) ne '-e'
    )
{
    if ($ARGV[0] eq '--verbose') {
        $verbose = 1;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-I') {
        shift @ARGV;
        my $lib = shift @ARGV;
        unshift @INC, $lib;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-I') {
        my $lib = substr($ARGV[0], 2);
        unshift @INC, $lib;
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-C') {
        $backend = substr($ARGV[0], 2);
        $execute = 0;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-MO=Deparse') {
        # this emulates perl -MO=Deparse option
        $backend = 'perl5';
        $execute = 0;
        $expand_use = 0;
        shift @ARGV;
    }
    elsif (uc(substr($ARGV[0], 0, 2)) eq '-M') {
        my $s = $ARGV[0];
        my $import = "";
        if (substr($s, 1, 1) eq "m") {
            $import = "()";
        }
        $s = substr($s, 2);
        my $use = "use";
        if (substr($s, 0, 1) eq "-") {
            $use = "no";
            $s = substr($s, 1);
        }
        if (index($s, "=") > -1) {
            ($s, $import) = split("=", $s);
            $import = "split(/,/,q{$import})";
        }
        push @Use, "$use $s $import";
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-w') {
        push @Use, "use warnings";
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-W') {
        push @Use, "use warnings";
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-X') {
        push @Use, "no warnings";
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-n') {
        if ($wrapper_priority < 1) {
            $wrapper_begin = ' LINE: while (<>) { ';
            $wrapper_end   = ' } ';
            $wrapper_priority = 1;
        }
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-p') {
        if ($wrapper_priority < 2) {
            $wrapper_begin = ' LINE: while (<>) { ';
            $wrapper_end   = ' } continue { '
                           .      ' print or die "-p destination: $!\n"; '
                           . ' } ';
            $wrapper_priority = 2;
        }
        chomp_switch();
    }
    elsif (($ARGV[0] eq '-V') || ($ARGV[0] eq '--version')) {
        $backend = '';
        say $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-v') {
        $backend = '';
        say $copyright_message;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-h' || $ARGV[0] eq '--help' || !@ARGV) {
        $backend = '';
        say $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION, $help_message;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--expand_use') {
        $expand_use = 1;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--noexpand_use') {
        $expand_use = 0;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--boilerplate') {
        $boilerplate = 1;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--noboilerplate') {
        $boilerplate = 0;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--bootstrapping') {
        $bootstrapping = 1;
        shift @ARGV;
    }
    else {
        die "Unrecognized switch: $ARGV[0]  (-h will show valid options).\n";
    }
}

if ($backend && @ARGV) {
    local $Perlito5::FILE_NAME = $ARGV[0];
    local $Perlito5::LINE_NUMBER = 1;
    if ($ARGV[0] eq '-e') {
        shift @ARGV;
        if ($verbose) {
            warn "// source from command line: ", $ARGV[0];
        }
        $source = shift @ARGV;
    }
    else {
        if ($verbose) {
            warn "// source from file: ", $ARGV[0];
        }
        my $source_filename = shift @ARGV;
        open FILE, '<:encoding(UTF-8)', $source_filename
          or die "Cannot read $source_filename: $!\n";
        local $/ = undef;
        $source = <FILE>;
        close FILE;
    }

    if ($verbose) {
        warn "// backend: ", $backend;
        warn "now parsing";
    }

    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};

    $source = "\n# line 1\n"
            . $source;

    if ($wrapper_begin) {
        $source = " $wrapper_begin;
                    $source;
                    $wrapper_end
                  ";
    }

    # TODO - reset information about the current compilation process,
    #        this should happen before the eval-string below is *compiled*.
    # our $BASE_SCOPE   = Perlito5::Grammar::Scope->new_base_scope();
    # our $SCOPE        = $BASE_SCOPE;    # information about the current block being compiled

    if ( $execute ) { 
        $Perlito5::EXPAND_USE = 1;
        local $@;
        my $init = join("; ", @Use);
        eval "  package main;
                $init;
                $source;
                \$@ = undef
            ";
        if ( $@ ) {
            my $error = $@;
            warn $error;
            exit(255);
        }
    }
    else {
        eval {
            # call the "ahead of time" compiler

            # since we are generating code that will run from scratch,
            # we need to start with an empty %INC so that all modules are "used"
            %INC = ();

            # partially disable "use"
            $Perlito5::EXPAND_USE = 0
                if $bootstrapping;

            # start with no-strict
            no strict;

            my $m;
            my $ok;
            eval {
                $m = Perlito5::Grammar::exp_stmts($source, 0);
                $ok = 1;
            };
            if (  !$ok
               || $m->{to} < length($source)
               )
            {
                my $error = $@
                    || (  $m->{to} < length($source)
                       && "Syntax Error near " . $m->{to}
                       )
                    || "Unknown error";
                warn $error;
                exit(255);
            }
            else {
                my $comp_units;
                if ($ENV{PERLITO5DEV}) {
                    # "new BEGIN"
                    $comp_units = Perlito5::Match::flat($m);
                }
                else {
                    # "old BEGIN"
                    if ($expand_use) {
                        my $ok;
                        eval {
                            $comp_units = Perlito5::Grammar::Use::add_comp_unit(Perlito5::Match::flat($m));
                            $ok = 1;
                        };
                        if ( !$ok ) {
                            my $error = $@
                                || "Unknown error loading a module";
                            warn $error;
                            exit(255);
                        }
                    }
                    else {
                        $comp_units = Perlito5::Match::flat($m);
                    }
                }

                $comp_units = [
                        Perlito5::AST::CompUnit->new(
                            name => 'main',
                            body => $comp_units,
                        ),
                    ];

                if ($backend eq 'perl5') {
                    say "# Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    if ( $expand_use ) {
                        print Perlito5::Perl5::Runtime->emit_perl5();
                    }
                    else {
                        $Perlito5::EMIT_USE = 1;
                    }
                    my @data = Perlito5::AST::CompUnit::emit_perl5_program( $comp_units );
                    # print Perlito5::Dumper::ast_dumper( \@data );
                    my $out = [];
                    Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                    print join( '', @$out ), ";1\n";
                }
                elsif ($backend eq 'perl6') {
                    if ($boilerplate) {
                        say "# Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                        say "use v6;";
                    }
                    if (!$boilerplate) {
                        # remove 'package main'
                        if (  ref($comp_units) eq 'ARRAY'
                           && (@$comp_units == 1)
                           && ref($comp_units->[0]) eq 'Perlito5::AST::CompUnit'
                           )
                        {
                            $comp_units = $comp_units->[0]{body};
                        }
                    }
                    my @data = Perlito5::AST::CompUnit::emit_perl6_program( $comp_units );
                    # print Perlito5::Dumper::ast_dumper( \@data );
                    my $out = [];
                    Perlito5::Perl6::PrettyPrinter::pretty_print( \@data, 0, $out );
                    print join( '', @$out );
                    print "\n" if $boilerplate;
                }
                elsif ($backend eq 'js') {
                    say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    print Perlito5::AST::CompUnit::emit_javascript2_program( $comp_units, expand_use => $expand_use );
                }
                elsif ($backend eq 'js3') {
                    say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    print Perlito5::AST::CompUnit::emit_javascript3_program( $comp_units, expand_use => $expand_use );
                }
                elsif ($backend eq 'xs') {
                    say "/* Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION, " */";
                    print Perlito5::AST::CompUnit::emit_xs_program( $comp_units );
                }
                elsif ($backend eq 'java') {
                    say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    print Perlito5::AST::CompUnit::emit_java_program( $comp_units, expand_use => $expand_use );
                }
                elsif ($backend eq 'ast-perl5') {
                    say Perlito5::Dumper::ast_dumper( $comp_units );
                }
                elsif ($backend eq 'ast-json') {
                    say Perlito5::JSON::ast_dumper( $comp_units );
                }
                elsif ($backend eq 'ast-pretty') {
                    eval 'use Data::Printer {colored=>1,class=>{expand=>"all",show_methods=>"none"}};p($comp_units);1';
                    print $@;
                }
                elsif ($backend eq '_comp') {
                    say Perlito5::Dumper::ast_dumper( $Perlito5::SCOPE );
                }
                elsif ($backend eq '_globals') {
                    # say Perlito5::CompileTime::Dumper::emit_globals_scope($Perlito5::SCOPE);
                    say Perlito5::CompileTime::Dumper::emit_globals($Perlito5::GLOBAL);
                }
                elsif ($backend eq '_compile_time') {
                    say Perlito5::Dumper::ast_dumper( Perlito5::AST::CompUnit::emit_compile_time_program( $comp_units ) );
                }
                else {
                    die "don't know what to do with backend '$backend'";
                }
            }
            $@ = undef;
        }
    }
    if ( $@ ) {
        my $error = $@;
        warn $error;
        exit(255);
    }
}

=pod

=head1 NAME

perlito5 - a Perl5 compiler

=head1 SYNOPSIS

    perlito5 -Isrc5/lib -Cjs program.pl

=head1 DESCRIPTION

This program reads Perl5 source code and generates native code.

The compiler options are available with the command:

    perlito5 -h

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

L<http://fglock.github.io/Perlito>

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://dev.perl.org/licenses/artistic.html>

=cut


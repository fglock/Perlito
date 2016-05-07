use v5;

package Perlito5;
use Perlito5::Match;
use Perlito5::Emitter::Token;
use Perlito5::Grammar::Expression;
use Perlito5::Grammar::Control;
use Perlito5::Grammar::Regex6;
use Perlito5::Grammar::Regex5;
use Perlito5::Grammar::Precedence;
use Perlito5::Grammar;
use Perlito5::Macro;
use Perlito5::Runtime;
use Perlito5::Dumper;

use Perlito5::JavaScript2::Emitter;
use Perlito5::JavaScript2::Runtime;
use Perlito5::JavaScript2::Array;
use Perlito5::JavaScript2::CORE;
use Perlito5::JavaScript2::IO;
use Perlito5::JavaScript2::Sprintf;

use Perlito5::JavaScript3::Emitter;
use Perlito5::JavaScript3::Runtime;
use Perlito5::JavaScript3::CORE;
use Perlito5::JavaScript3::IO;
use Perlito5::JavaScript3::Sprintf;

use Perlito5::Perl5::Emitter;
use Perlito5::Perl5::PrettyPrinter;
use Perlito5::Perl5::Runtime;

use Perlito5::Perl6::Emitter;
use Perlito5::Perl6::PrettyPrinter;

use Perlito5::XS::Emitter;      # experimental
## use Perlito5::Python::Emitter;
## use Perlito5::Ruby::Emitter;

use strict;

my $_V5_COMPILER_NAME    = 'Perlito5';
my $_V5_COMPILER_VERSION = '9.0';
my $source      = '';
my $backend     = $^O;
my $execute     = 1;
my $verbose     = 0;
my $expand_use  = 1;
my $boilerplate = 1;

if ($verbose) {
    warn "// Perlito5 compiler";
    warn "// ARGV: @ARGV";
}

my $help_message = "
perlito5 [switches] [programfile]
  switches:
    -h --help
    --verbose
    -V --version
    -v
    -Idirectory     specify \@INC/include directory (several -I's allowed)
    -Ctarget        target backend: js, perl5, perl6, xs
    -Cast-perl5     emits a dump of the abstract syntax tree
    --expand_use --noexpand_use
                    expand 'use' statements at compile time
    --boilerplate --noboilerplate
                    emits or not boilerplate code
    -e program      one line of program (omit programfile)
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
        $backend = substr($ARGV[0], 2, 10);
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
    else {
        die "Unrecognized switch: $ARGV[0]  (-h will show valid options).\n";
    }
}

if ($backend && @ARGV) {
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
        $source = Perlito5::IO::slurp( shift @ARGV );
    }

    if ($verbose) {
        warn "// backend: ", $backend;
        warn "now parsing";
    }

    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};

    {
        eval {
            # call the "ahead of time" compiler

            # since we are generating code that will run from scratch,
            # we need to start with an empty %INC so that all modules are "used"
            %INC = ();

            # partially disable "use"
            $Perlito5::EXPAND_USE = 0;

            # start with no-strict
            no strict;

            my $m;
                $m = Perlito5::Grammar::Regex5->rule($source, 0);
                my $error = $@
                    || (  $m->{to} != length($source)
                       && "Syntax Error near " . $m->{to}
                       )
                    || "Unknown error";
                warn $error;
            {
                my $comp_units;
                $comp_units = Perlito5::Match::flat($m);

                if ($backend eq 'perl5') {
                    say "# Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    if ( $expand_use ) {
                        print Perlito5::Perl5::Runtime->emit_perl5();
                    }
                    my @data = Perlito5::AST::CompUnit::emit_perl5_program( $comp_units );
                    # print Perlito5::Dumper::ast_dumper( \@data );
                    my $out = [];
                    Perlito5::Perl5::PrettyPrinter::pretty_print( \@data, 0, $out );
                    print join( '', @$out ), "\n";
                }
                if ($backend eq 'perl6') {
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
                if ($backend eq 'js') {
                    say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    if ( $expand_use ) {
                        print Perlito5::JavaScript2::Runtime->emit_javascript2();
                        print Perlito5::JavaScript2::Array->emit_javascript2();
                        print Perlito5::JavaScript2::CORE->emit_javascript2();
                        print Perlito5::JavaScript2::IO->emit_javascript2();
                        print Perlito5::JavaScript2::Sprintf->emit_javascript2();
                    }
                    print Perlito5::AST::CompUnit::emit_javascript2_program( $comp_units );
                }
                if ($backend eq 'js3') {
                    say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                    if ( $expand_use ) {
                        print Perlito5::JavaScript3::Runtime->emit_javascript3();
                        print Perlito5::JavaScript3::CORE->emit_javascript3();
                        print Perlito5::JavaScript3::IO->emit_javascript3();
                        print Perlito5::JavaScript3::Sprintf->emit_javascript3();
                    }
                    print Perlito5::AST::CompUnit::emit_javascript3_program( $comp_units );
                }
                if ($backend eq 'xs') {
                    say "/* Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION, " */";
                    # if ( $expand_use ) {
                    #    print Perlito5::XS::Runtime->emit_xs();
                    # }
                    print Perlito5::AST::CompUnit::emit_xs_program( $comp_units );
                }
                # if ($backend eq 'java') {
                #     say "// Do not edit this file - Generated by ", $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
                #     print Perlito5::AST::CompUnit::emit_java_program( $comp_units );
                # }
                if ($backend eq 'ast-perl5') {
                    say Perlito5::Dumper::ast_dumper( $comp_units );
                }
                elsif ($backend eq 'ast-pretty') {
                    eval 'use Data::Printer {colored=>1,class=>{expand=>"all",show_methods=>"none"}};p($comp_units);1';
                    print $@;
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

perlito5 - Perl5 compiler

=head1 SYNOPSIS

    perlito5 -Cjs program.pl

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

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut


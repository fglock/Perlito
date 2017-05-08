#! /usr/bin/perl

use v5;

package Perlito5;
use feature 'say';
use Perlito5;
use Perlito5::Compiler;
use Perlito5::CompileTime::Dumper;
use Perlito5::Grammar::Regex6;
use Perlito5::Emitter::Token;
use Perlito5::Dumper;
use Perlito5::JSON;

use Perlito5::JavaScript2::Emitter;
use Perlito5::JavaScript2::Runtime;
use Perlito5::JavaScript2::Lib;
use Perlito5::JavaScript2::Array;
use Perlito5::JavaScript2::CORE;
use Perlito5::JavaScript2::IO;
use Perlito5::JavaScript2::Sprintf;

# use Perlito5::JavaScript3::Emitter;
# use Perlito5::JavaScript3::Runtime;
# use Perlito5::JavaScript3::CORE;
# use Perlito5::JavaScript3::IO;
# use Perlito5::JavaScript3::Sprintf;

use Perlito5::Perl5::Emitter;
use Perlito5::Perl5::PrettyPrinter;
use Perlito5::Perl5::Runtime;

use Perlito5::Perl6::Emitter;
use Perlito5::Perl6::PrettyPrinter;

# use Perlito5::XS::Emitter;      # experimental

use Perlito5::Java::Emitter;
use Perlito5::Java::Runtime;
use Perlito5::Java::Lib;

## use Perlito5::Python::Emitter;
## use Perlito5::Ruby::Emitter;

use strict;

my $_V5_COMPILER_NAME    = Perlito5::Compiler::compiler_name;
my $_V5_COMPILER_VERSION = $Perlito5::VERSION;
my $source      = '';
my $backend     = $^O;
my $compile_only = 0;
my $execute     = 1;
my $verbose     = 0;
my $expand_use  = 1;
my $boilerplate = 1;
my $bootstrapping = 0;
my $wrapper_begin = '';
my $wrapper_end = '';
my $wrapper_priority = 0;
my @Use;
my $i_switch = 0;
my $i_switch_extension = '';
my @e_switch;
$Perlito5::BOOTSTRAP_JAVA_EVAL = 0;
$Perlito5::JAVA_EVAL = 0;
$Perlito5::FILE_NAME = '';

if ($verbose) {
    warn "// Perlito5 compiler";
    warn "// ARGV: @ARGV";
}

my $help_message = "
perlito5 [switches] [programfile]
  switches:
    -c              check syntax only (runs BEGIN and CHECK blocks)
    -e program      one line of program (omit programfile)
    -E program      like -e, but enables all optional features
    -h --help
    -Idirectory     specify \@INC/include directory (several -I's allowed)
    -[mM][-]module  execute \"use/no module...\" before executing program
    -n              assume \"while (<>) { ... }\" loop around program
    -p              assume loop like -n but print line also, like sed
    -V --version
    -v
    --verbose
    -Ctarget        target backend: js, perl5, perl6, java
    -Cast-perl5     emits a dump of the abstract syntax tree as a Perl dump
    -Cast-json      emits a dump of the abstract syntax tree in JSON format
    --expand_use --noexpand_use
                    expand 'use' statements at compile time
    --boilerplate --noboilerplate
                    emits or not boilerplate code
    --bootstrapping set this when compiling the compiler,
                    otherwise the new subroutine definitions will overwrite the current compiler
    --java_eval     enable java eval (experimental)
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

sub get_text_from_switch {
    # process [[ -I'.' ]] into [[ . ]]
    my $s = substr($ARGV[0], 2);
    if (!$s) {
        shift @ARGV;
        $s = $ARGV[0];
    }
    if ($s) {
        my $c = substr($s,0,1);
        if ($c eq '"' || $c eq "'") {
            if (substr($s,-1,1) eq $c) {
                $s = substr($s,1,-1);
            }
        }
    }
    return $s;
}

my $use_warnings = "";
push @Use, "no strict";

ARG_LOOP:
while (@ARGV && substr($ARGV[0], 0, 1) eq '-')
{
    if ($ARGV[0] eq '--verbose') {
        $verbose = 1;
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-I') {
        my $lib = get_text_from_switch();
        unshift @INC, $lib;
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-e' || substr($ARGV[0], 0, 2) eq '-E') {
        my $source = get_text_from_switch();
        push @e_switch, $source;
        $Perlito5::FILE_NAME = '-e';
        if ($verbose) {
            warn "// source from command line: $source";
        }
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-c') {
        $compile_only = 1;
        $execute = 0;
        $backend = 'perl5';
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-C') {
        $backend = get_text_from_switch();
        $execute = 0;
        shift @ARGV;
    }
    elsif (substr($ARGV[0], 0, 2) eq '-i') {
        $i_switch = 1;
        $i_switch_extension = get_text_from_switch() if $ARGV[0] ne '-i';
        die "switch -i $i_switch_extension not yet implemented.\n";
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
        $use_warnings = "w";
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-W') {
        $use_warnings = "W";
        chomp_switch();
    }
    elsif (substr($ARGV[0], 0, 2) eq '-X') {
        $use_warnings = "";
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
    elsif ($ARGV[0] eq '-V') {
        $backend = '';
        say $_V5_COMPILER_NAME, " ", $_V5_COMPILER_VERSION;
        if ($ENV{PERL5LIB}) {
            say '  %ENV:';
            say qq{    PERL5LIB="$ENV{PERL5LIB}"};
        }
        say '  @INC:';
        say qq{    $_} for @INC;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-v' || $ARGV[0] eq '--version') {
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
    elsif ($ARGV[0] eq '--bootstrap_java_eval') {
        $Perlito5::BOOTSTRAP_JAVA_EVAL = 1;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '--java_eval') {
        $Perlito5::JAVA_EVAL = 1;
        shift @ARGV;
    }
    elsif ($ARGV[0] eq '-') {
        shift @ARGV;
        last ARG_LOOP;
    }
    else {
        die "Unrecognized switch: $ARGV[0]  (-h will show valid options).\n";
    }
}

if (!$expand_use) {
    $Perlito5::EMIT_USE = 1;
}

if ($backend) {
    local $Perlito5::LINE_NUMBER = 1;
    if (@e_switch) {
        $source = "@e_switch";
    }
    else {
        $Perlito5::FILE_NAME = $ARGV[0];
        if ($verbose) {
            warn "// source from file: ", $ARGV[0];
        }
        my $source_filename = shift @ARGV;
        if ($source_filename eq '') {
            local $/ = undef;
            $source = <STDIN>;
        }
        else {
            open FILE, '<:encoding(UTF-8)', $source_filename
              or die "Cannot read $source_filename: $!\n";
            local $/ = undef;
            $source = <FILE>;
            close FILE;
        }
    }
    $0 = $Perlito5::FILE_NAME;

    if ($verbose) {
        warn "// backend: ", $backend;
        warn "now parsing";
    }

    $Perlito5::PKG_NAME = 'main';
    $Perlito5::PROTO    = {};
    Perlito5::set_global_phase("BEGIN");

    if ($source =~ /^#![^\n]+-(w|W)/) {
        #!./perl -w
        $use_warnings = $1;
    }

    $source = "\n# line 1\n"
            . $source;

    if ($wrapper_begin) {
        $source = " $wrapper_begin;
                    $source;
                    $wrapper_end
                  ";
    }
    if ($verbose) {
        warn "// source [[[ $source ]]]\n";
    }

    # TODO - reset information about the current compilation process,
    #        this should happen before the eval-string below is *compiled*.
    # our $BASE_SCOPE   = Perlito5::Grammar::Scope->new_base_scope();
    # our $SCOPE        = $BASE_SCOPE;    # information about the current block being compiled

    Perlito5::Java::Lib::init()
        if $backend eq 'java';
    Perlito5::JavaScript2::Lib::init()
        if $backend eq 'js' || $^O eq 'node.js';

    # work around java code size limitation
    $Perlito5::CODE_TOO_LARGE = 1
        if $backend eq 'java';

    $Perlito5::EXPAND_USE = 1;
    # partially disable "use":
    # force "use" code to be inlined instead of eval-ed
    $Perlito5::EXPAND_USE = 0
        if $bootstrapping;

    if ( $execute ) { 
        local $@;
        my $init = join("; ", @Use);
        my $warnings = '';
        $warnings = "use warnings" if $use_warnings;
        eval qq{
            $warnings;
            Perlito5::set_global_phase("CHECK");
            \$_->() for \@Perlito5::CHECK_BLOCK;
            package main;
            $init;
            Perlito5::set_global_phase("INIT");
            eval {
                \$_->() for \@Perlito5::INIT_BLOCK;
                1;
            }
            or die "\$@\nINIT failed--call queue aborted.\n";
            Perlito5::set_global_phase("RUN");
            $source;
            \$@ = undef
        };
        my $error = $@;
        warn $error if $error;
        Perlito5::set_global_phase("END");
        $_->() for @Perlito5::END_BLOCK;
        if ( $error ) {
            exit(255);
        }
    }
    else {
        eval {
            # call the "ahead of time" compiler

            # since we are generating code that will run from scratch,
            # we need to start with an empty %INC so that all modules are "used"
            %INC = ();

            @Perlito5::COMP_UNIT = ();

            # start with no-strict
            no strict;

            $source = "use warnings;\n" . $source if $use_warnings;

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
                if ($expand_use) {
                    my $ok;
                    eval {
                        push @Perlito5::COMP_UNIT,
                          Perlito5::AST::CompUnit->new(
                            name => 'main',
                            body => Perlito5::Match::flat($m),
                          );
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
                    push @Perlito5::COMP_UNIT, Perlito5::Match::flat($m);
                }

                for (0 .. $#Perlito5::COMP_UNIT) {
                    # use lexicals from BEGIN scratchpad
                    $Perlito5::COMP_UNIT[$_] = $Perlito5::COMP_UNIT[$_]->emit_begin_scratchpad();
                }

                {
                    local ${^GLOBAL_PHASE};
                    Perlito5::set_global_phase("CHECK");
                    $_->() for @Perlito5::CHECK_BLOCK;
                }

                if (!$bootstrapping) {
                    # emit BEGIN-block side-effects, INIT blocks
                    $Perlito5::STRICT = 0;
                    my @units;
                    push @units,
                        Perlito5::AST::Block->new(
                            stmts => Perlito5::CompileTime::Dumper::emit_globals_after_BEGIN($Perlito5::GLOBAL),
                        );
                    if (@Perlito5::INIT_BLOCK || keys %Perlito5::DATA_SECTION) {
                        $s = '{ ';
                        if (keys %Perlito5::DATA_SECTION) {
                            for my $pkg (keys %Perlito5::DATA_SECTION) {
                                # open(main::DATA, '<', \$Perlito5::DATA_SECTION{main}{data});
                                # seek(main::DATA, $Perlito5::DATA_SECTION{main}{pos}, 0);
                                $s .= q%open % . $pkg . q%::DATA, '<', \$Perlito5::DATA_SECTION{% . $pkg . q%}{data}; %;
                                $s .= q%seek(% . $pkg . q%::DATA, $Perlito5::DATA_SECTION{% . $pkg . q%}{pos}, 0); %;
                            }
                        }
                        $s .=
                            'local $@; '
                        .   'local ${^GLOBAL_PHASE}; '
                        .   'eval { ${^GLOBAL_PHASE} = "INIT" }; '    # GLOBAL_PHASE is r/o in perl5
                        .   'eval { '
                        .       '$_->() for @Perlito5::INIT_BLOCK; '  # execute INIT blocks
                        .       '1; '
                        .   '} '
                        .   'or die "$@\nINIT failed--call queue aborted.\n"; '
                        . '} ';
                        my $m = Perlito5::Grammar::exp_stmts($s, 0);
                        push @units, @{ Perlito5::Match::flat($m) };
                    }
                    unshift @Perlito5::COMP_UNIT, @units;


                    # TODO - insert END block executor
                    # NOTE - END blocks are currently processed by P::Java::Emitter instead

                    # my $error;
                    # eval {
                    #   ... whole program
                    #   1;
                    # }
                    # or $error = $@;
                    # warn $error if $error;
                    # Perlito5::set_global_phase("END");
                    # eval {
                    #     $_->() for @Perlito5::END_BLOCK;
                    #     1;
                    # }
                    # or warn "$@\nEND failed--call queue aborted.\n"
                    # if ( $error ) {
                    #     exit(255);
                    # }

                }

                my $comp_units = [ @Perlito5::COMP_UNIT ];

                if ($compile_only) {
                    # -c switch
                    say $Perlito5::FILE_NAME . " syntax OK";
                }
                # elsif ($backend eq 'eval') {
                #     my $block =
                #         Perlito5::AST::Block->new(
                #             stmts => [ map { @{ $_->{body} } } @$comp_units ],
                #         );
                #     Perlito5::eval_ast($block);
                # }
                elsif ($backend eq 'perl5') {
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
                    print Perlito5::AST::CompUnit::emit_javascript2_program( $comp_units, expand_use => $expand_use );
                }
                # elsif ($backend eq 'js3') {
                #     print Perlito5::AST::CompUnit::emit_javascript3_program( $comp_units, expand_use => $expand_use );
                # }
                # elsif ($backend eq 'xs') {
                #     print Perlito5::AST::CompUnit::emit_xs_program( $comp_units );
                # }
                elsif ($backend eq 'java') {
                    print Perlito5::AST::CompUnit::emit_java_program( $comp_units, expand_use => $expand_use );
                }
                elsif ($backend eq 'ast-perl5' || $backend eq 'ast') {
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

L<Perlito5>

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://dev.perl.org/licenses/artistic.html>

=cut


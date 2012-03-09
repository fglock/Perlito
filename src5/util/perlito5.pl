use v5;

package Perlito;
use Perlito5::Match;
use Perlito5::Emitter::Token;
use Perlito5::Expression;
use Perlito5::Grammar::Control;
use Perlito5::Grammar::Regex;
use Perlito5::Grammar;
use Perlito5::Javascript::Emitter;
use Perlito5::Javascript::Runtime;
use Perlito5::Javascript::CORE;
use Perlito5::Macro;
use Perlito5::Perl5::Emitter;
use Perlito5::Perl6::Emitter;
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
my $perl5lib    = './src5/lib';
my $expand_use  = 1;

if ($verbose) {
    warn "// Perlito5 compiler";
    warn "// ARGV: @ARGV";
}

my %module_seen;

sub modulename_to_filename {
    my $s = shift;
    return Perlito5::Runtime::_replace( $s, '::', '/' );
}

sub expand_use {
    my $stmt = shift;
    my $module_name = $stmt->mod;
    return
        if $module_name eq 'v5'
        || $module_name eq 'strict'
        || $module_name eq 'feature';
    if (!($module_seen{$module_name})) {
        $module_seen{$module_name} = 1;
        # say "  now use: ", $module_name;
        if ($backend eq 'perl5' || $backend eq 'ast-perl5') {
            # skip 'use' statements for this backend
        }
        else {
            # TODO - look for a precompiled version
            # build the filename
            my $filename = $module_name;
            $filename = $perl5lib . '/' . modulename_to_filename($filename) . '.pm';
            if ( $verbose ) {
                warn "// now loading: ", $filename;
            }
            # load source
            my $source = Perlito5::IO::slurp( $filename );

            # compile; push AST into comp_units
            # warn $source;
            my $m = Perlito5::Grammar->exp_stmts($source, 0);
            die "Syntax Error near ", $m->{"to"}
                if $m->{"to"} != length($source);
            add_comp_unit(
                [
                    Perlito5::AST::CompUnit->new(
                        name => 'main',
                        body => $m->flat(),
                    )
                ]
            );
        }
    }
}

sub add_comp_unit {
    my $parse = shift;
    for my $comp_unit (@$parse) {
        if ($expand_use && $comp_unit->isa('Perlito5::AST::Use')) {
            expand_use($comp_unit);
        }
        elsif ($comp_unit->isa('Perlito5::AST::CompUnit')) {
            if ($verbose) {
                warn "parsed comp_unit: '", $comp_unit->name, "'";
            }
            for my $stmt (@{ $comp_unit->body }) {
                if ($expand_use && $stmt->isa('Perlito5::AST::Use')) {
                    expand_use($stmt);
                }
            }
        }
        push @$comp_units, $comp_unit;
        # say "comp_unit done";
    }
}

    if (($ARGV[0] eq '-v') || ($ARGV[0] eq '--verbose')) {
        $verbose = 1;
        shift @ARGV;
    }
    if (substr($ARGV[0], 0, 2) eq '-C') {
        $backend = substr($ARGV[0], 2, 10);
        $execute = 0;
        shift @ARGV;
        if (  $backend eq 'perl5' 
           || $backend eq 'python' 
           || $backend eq 'ruby'
           || $backend eq 'perl6'
           )
        {
            $expand_use = 0;
        }
    }
    if (substr($ARGV[0], 0, 2) eq '-B') {
        $backend = substr($ARGV[0], 2, 10);
        $execute = 1;
        shift @ARGV;
        if (($backend eq 'perl5') || ($backend eq 'python') || ($backend eq 'ruby')) {
            $expand_use = 0;
        }
    }
    if (($ARGV[0] eq '-V') || ($ARGV[0] eq '--version')) {
        $backend = '';
        say $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
        shift @ARGV;
    }
    elsif (($ARGV[0] eq '-h') || ($ARGV[0] eq '--help') || ($backend eq '')) {
        $backend = '';
        say $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION, "
perlito5 [switches] [programfile]
  switches:
    -h --help
    -v --verbose
    -V --version
    -Ctarget        target backend: js, perl5, perl6
    --expand_use --noexpand_use
                    expand 'use' statements at compile time
    -e program      one line of program (omit programfile)
";
        shift @ARGV;
    }
    if ($ARGV[0] eq '--expand_use') {
        $expand_use = 1;
        shift @ARGV;
    }
    if ($ARGV[0] eq '--noexpand_use') {
        $expand_use = 0;
        shift @ARGV;
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
        my $m = Perlito5::Grammar->exp_stmts($source, 0);
        die "Syntax Error near ", $m->{"to"}
            if $m->{"to"} != length($source);
        add_comp_unit($m->flat());

        $comp_units = [
                Perlito5::AST::CompUnit->new(
                    name => 'main',
                    body => $comp_units,
                ),
            ];

        # if ($backend eq 'ast-perl5') {
        #     say "# AST dump - do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
        #     # TODO - use Data::Dumper
        #     say "$comp_units";
        # }
        if ($backend eq 'perl5') {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            print Perlito5::AST::CompUnit::emit_perl5_program( $comp_units );
        }
        if ($backend eq 'perl6') {
            say "# Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
            print Perlito5::AST::CompUnit::emit_perl6_program( $comp_units );
        }
        if ($backend eq 'js') {
            say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;

            if ( $expand_use ) {
                print Perlito5::Javascript::Runtime->emit_javascript();
                print Perlito5::Javascript::CORE->emit_javascript();
            }
            print Perlito5::AST::CompUnit::emit_javascript_program( $comp_units );
        }
        # if ($backend eq 'java') {
        #     say "// Do not edit this file - Generated by ", $_V6_COMPILER_NAME, " ", $_V6_COMPILER_VERSION;
        #     print Perlito5::AST::CompUnit::emit_java_program( $comp_units );
        # }
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

L<http://www.perlito.org>

=head1 COPYRIGHT

Copyright 2011, 2012 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut


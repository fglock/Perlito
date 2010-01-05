package main;

use FindBin '$Bin';
use lib ("$Bin/lib5");
use strict;

BEGIN {
    $::_V6_COMPILER_NAME    = 'MiniPerl6';
    $::_V6_COMPILER_VERSION = '2.0';
}

use MiniPerl6::Perl5::Runtime;
use MiniPerl6::Perl5::Match;

package Main;
use MiniPerl6::Perl5::Emitter;
use MiniPerl6::Grammar;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

my ( @switch_e, $source, $source_filename, $result );
my @comp_unit;
my $backend = 'perl5';
my $tmp_filename = 'tmp';
my $execute = 1;
my @args = @ARGV;
while (@args) {
    if ( $args[0] eq '-B' || $args[0] eq '-C' ) {
        if ( @args > 1 ) {
            $args[1] = $args[0] . $args[1];
            shift @args;
        }
        else {
            die("Missing argument for $args[0] option");
        }
    } 
    if ( $args[0] =~ /^-B(.*)/ ) {
        $execute = 1;
        $backend = $1;
        shift @args;
        redo;
    }
    if ( $args[0] =~ /^-C(.*)/ ) {
        $execute = 0;
        $backend = $1;
        shift @args;
        redo;
    }
    if ( $args[0] eq '-e' ) {
        my ($switch, $source) = (shift @args, shift @args);
        push @switch_e, $source;
        redo;
    }
    last;
}

$source_filename = shift @args if @args;

if (@switch_e) {
    $source = join('; ', @switch_e);
}
elsif ($source_filename) {
    open FILE, $source_filename
      or die "Cannot read $source_filename\n";
    local $/ = undef;
    $source = <FILE>;
    close FILE;
}
else {
    local $/ = undef;
    $source = <STDIN>;
}

if ( $source_filename =~ /\.p5ast$/ ) {
    # source code was precompiled to AST, dumped as a perl5 structure
    @comp_unit = @{ eval $source };
}
else {
    # Kludge - make an implicit Main explicit.
    $source = "class Main { $source }" if $source !~ /class/;
    my $pos = 0;
    while ( $pos < length($source) ) {
        my $p = MiniPerl6::Grammar->comp_unit( $source, $pos );
        push @comp_unit, $$p;
        $pos = $p->to;
    }
}

if ( $backend eq 'lisp' ) {
    require MiniPerl6::Lisp::Emitter;
    $result .=  ";; Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_lisp() . "\n";
    }

    if ( $execute ) {
        die "execute Lisp not implemented\n";
    }
}
if ( $backend eq 'js' ) {
    require MiniPerl6::Javascript::Emitter;
    $result .=  "// Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_javascript() . "\n";
    }

    if ( $execute ) {
        die "execute Javascript not implemented\n";
    }
}
if ( $backend eq 'go' ) {
    require MiniPerl6::Go::Emitter;
    $result .=  "// Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";

    my $lib_source_filename = 'lib/MiniPerl6/Go/Runtime.go';
    $result .= "// include file: $lib_source_filename\n";
    open FILE, $lib_source_filename
      or die "Cannot read $lib_source_filename\n";
    local $/ = undef;
    $result .= <FILE>;
    close FILE;
    $result .= "// end include file: $lib_source_filename\n";

    $result .= CompUnit::emit_go_program( \@comp_unit );

    if ( $execute ) {
        die "execute Go not implemented\n";
    }
}
if ( $backend eq 'perl5' ) {
    $result .=  "# Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    $result .=  "use v5;\n";
    $result .=  "use strict;\n";
    $result .=  "use MiniPerl6::Perl5::Runtime;\n";
    $result .=  "use MiniPerl6::Perl5::Match;\n";
    for my $p ( @comp_unit ) {
        $result .=  "{\n" . $p->emit() . "}\n";
    }
    $result .=  "1;\n";

    if ( $execute ) {
        eval $result;
        warn $@ if $@;
    }
}
if ( $backend eq 'ast-perl5' ) {
    require Data::Dumper;
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Indent   = 1;
    $result .=  Data::Dumper::Dumper( \@comp_unit );
}

if ( !$execute ) {
    print $result;
}

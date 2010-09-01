#!/usr/local/bin/perl
package main;

use FindBin '$Bin';
use lib ("$Bin/lib5");
use strict;
use Encode;

BEGIN {
    $::_V6_COMPILER_NAME    = 'Perlito';
    $::_V6_COMPILER_VERSION = '6.0';
}

use Perlito::Perl5::Runtime;

package Main;
use Perlito::Perl5::Emitter;
use Perlito::Grammar;
use Perlito::Grammar::Regex;
use Perlito::Emitter::Token;

my ( @switch_e, $source, $source_filename, $result );
my @comp_unit;
my $backend = 'perl5';
my $tmp_filename = 'tmp';
my @cmd;
my $execute = 1;
my $compile_to_bin = 0;
my $verbose = 0;
my $lib_spec = '';
my @args = @ARGV;
while (@args) {
    if ( $args[0] eq '-v' || $args[0] eq '--verbose' ) {
        $verbose = 1;
        shift @args;
        redo;
    }
    if ( $args[0] eq '-V' || $args[0] eq '--version' ) {
        print "$::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
        exit;
    }
    if ( $args[0] eq '-h' || $args[0] eq '--help') {
        print "$::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION
mp6 [switches] [programfile]
  switches:
    -h --help
    -v --verbose
    -V --version
    -e program      one line of program (omit programfile)
    -Ctarget        compile to target backend: go, js, lisp, parrot, perl5, python, ruby
        options:
        -Cgo           compile to Go source code
        -Cjs           compile to Javascript source code
        -Clisp         compile to Lisp source code
        -Cparrot       compile to PIR source code
        -Cperl5        compile to Perl 5 source code
        -Cpython       compile to Python source code
        -Cruby         compile to Ruby source code
        -Cast-perl6    dump the ast in Perl 6 format
        -Cast-json     dump the ast in JSON format
        -Cast-perl5    dump the ast in Perl 5 format
        -Cgo-bin       create a binary executable file using Go (doesn't run it)
        -Clisp-bin     create a binary executable file using SBCL Lisp (doesn't run it) 
        -Cjava-class   create a Java .class (doesn't run it)
    -Btarget        run in target backend: go, js, lisp, parrot, perl5
        options:
        -Bgo           run in Go (this also creates a binary executable)
        -Bjs           run in Javascript using the \"js\" command (Spidermonkey or V8)
        -Blisp         run in SBCL (Lisp)
        -Bparrot       run in Parrot
        -Bperl5        run in Perl 5 
        -Bpython       run in Python 
        -Bpython3      run in Python 3
        -Bruby         run in Ruby 
        -Bruby1.9      run in Ruby 1.9
        -Brhino        run in JVM using Rhino
        -Bv8           run in V8 (Javascript) using the \"v8\" command 
        -Bspidermonkey run in SpiderMonkey (Javascript) using the \"spidermonkey\" command
";
        exit;
    }
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
    if ( $args[0] =~ /^-C(.*?)(-bin)?$/ ) {
        $execute = 0;
        $backend = $1;
        $compile_to_bin = $2 ? 1 : 0;
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

if ( $backend eq 'js'    ) { 
    @cmd = ('js'); 
    $lib_spec = 'Javascript';
}
if ( $backend eq 'java-class' ) { 
    @cmd = qw/java org.mozilla.javascript.tools.jsc.Main -opt 9/;
    $backend = 'js';
    $lib_spec = 'Javascript';
    $execute = 1;
}
if ( $backend eq 'rhino' ) { 
    @cmd = qw/java org.mozilla.javascript.tools.shell.Main/; 
    $backend = 'js';
    $lib_spec = 'Javascript';
}
if ( $backend eq 'spidermonkey' ) { 
    @cmd = qw/spidermonkey/; 
    $backend = 'js';
    $lib_spec = 'Javascript';
}
if ( $backend eq 'v8'    ) { 
    @cmd = ('v8');    
    $backend = 'js';
    $lib_spec = 'Javascript';
}
if ( $backend eq 'go' ) {
    $lib_spec = 'Go';
}
if ( $backend eq 'lisp' ) {
    $lib_spec = 'Lisp';
}
if ( $backend eq 'python' ) {
    @cmd = ('python');
    $backend = 'python';
    $lib_spec = 'Python';
}
if ( $backend eq 'python3' ) {
    @cmd = ('python3');
    $backend = 'python';
    $lib_spec = 'Python';
}
if ( $backend eq 'ruby' ) {
    @cmd = ('ruby');
    $backend = 'ruby';
    $lib_spec = 'Ruby';
}
if ( $backend eq 'ruby1.9' ) {
    @cmd = ('ruby1.9');
    $backend = 'ruby';
    $lib_spec = 'Ruby';
}

$source_filename = shift @args if @args;

if ( $verbose ) {
    warn "compilation parameters:\n";
    warn "\tbackend         '$backend'\n";
    warn "\ttmp_filename    '$tmp_filename'\n";
    warn "\texecute         '$execute'\n";
    warn "\tsource_filename '$source_filename'\n";
    warn "\tBin             '$::Bin'\n";
    warn "\tcmd             '@cmd'\n";
    warn "\tlibspec         '$lib_spec'\n";
    warn "\te               '${_}'\n" for @switch_e;
}

if (@switch_e) {
    $source = join('; ', @switch_e);
}
elsif ($source_filename) {
    open FILE, $source_filename
      or die "Cannot read $source_filename\n";
    local $/ = undef;
    $source = <FILE>;
    close FILE;
    warn "read " . length($source) . " chars from $source_filename\n" if $verbose;
}
else {
    warn "reading input from STDIN\n" if $verbose;
    local $/ = undef;
    $source = <STDIN>;
}
$source = Encode::decode_utf8($source);

if ( $source_filename =~ /\.p5ast$/ ) {
    # source code was precompiled to AST, dumped as a perl5 structure
    warn "input format is precompiled AST\n" if $verbose;
    @comp_unit = @{ eval $source };
}
else {
    if  (  $backend eq 'go' 
        || $backend eq 'js'
        || ( $backend eq 'lisp' && ( $execute || $compile_to_bin ) )
        ) 
    {
        # TODO - recursive 'use'
        my %module;
        my $precompiled;
        my $load_module = sub {
            my $module_name = shift;
            warn "load module: $module_name\n" if $verbose;
            return 1 if $module{$module_name};
            my $filename = $module_name;
            $filename =~ s{::}{/}g;
            $filename = $::Bin . "/libast-perl5/${filename}.p5ast";
            for (1) {
                my $has_ast = open FILE, $filename;
                if ( $has_ast ) {
                    # reuse Prelude AST if available
                    local $/ = undef;
                    my $ast = <FILE>;
                    close FILE;
                    if ( length($ast) ) {
                        push @comp_unit, @{ eval $ast };
                        warn "Error loading $filename: $@" if $@;
                        warn "included $module_name ast\n" if $verbose;
                    }
                    else {
                        $has_ast = 0;
                        close(FILE);
                        warn "$filename is empty, removing broken file";
                        unlink $filename;
                    }
                }
                else {
                    if ( !$precompiled) {
                        warn "now compiling Perlito source code to AST\n" if $verbose;
                        mkdir $::Bin . "/libast-perl5-new";  # avoid error message in the script below
                        system( ". util-perl5/update-ast-perl5.sh" );
                        $precompiled = 1;
                        redo;
                    }
                    die "can't load module $module_name. Looking in: $filename\n";
                }
            }
            return 1;
        };
        my $pos = 0;
        my $module = "Perlito::${lib_spec}::Prelude";
        $load_module->($module);
        my $p = Perlito::Grammar->exp_stmts( $source, $pos );
        if (!$p || $p->to < length($source)) {
            die "Syntax error at pos ", $p->to, " in $module\n";
        }
        warn "matched source code to ", $p->to, " in $module\n" if $verbose;
        for my $use (  
            map  { $_->{mod} } 
            grep { $_->isa("Use") } 
            map  { @{$_->{body}} } 
            grep { $_->isa("CompUnit") } @{$$p} )
        {
            $load_module->($use);
        }
        push @comp_unit, 
            CompUnit->new(
                name => 'GLOBAL',
                body => $$p,
            );
    }
    else {
        my $pos = 0;
        my $p = Perlito::Grammar->exp_stmts( $source, $pos );
        if (!$p || $p->to < length($source)) {
            die "Syntax error at pos ", $p->to, "\n";
        }
        warn "matched source code to ", $p->to, "\n" if $verbose;
        push @comp_unit,
            CompUnit->new(
                name => 'GLOBAL',
                body => $$p,
            );
    }
}

warn "starting emitter phase\n" if $verbose;
if ( $backend eq 'ruby' ) {
    require Perlito::Ruby::Emitter;
    $result .= "# Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    $result .= "\n";
    $result .= "require 'Perlito/Ruby/Runtime.rb'\n";
    $result .= "\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_ruby() . "\n";
    }
    if ( $execute || $compile_to_bin ) {
        open( OUT, '>', $tmp_filename . '.rb' )
          or die "Cannot write to ${tmp_filename}.rb\n";
        print OUT $result, "\n";
        close(OUT);
        local $ENV{RUBYLIB} = 'librb';
        exec( @cmd, "$tmp_filename.rb", @args )
            or die "can't execute";
    }
}
elsif ( $backend eq 'python' ) {
    require Perlito::Python::Emitter;
    $result .= "# Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    $result .= "\n";
    $result .= "from perlito.python.runtime import *\n";
    $result .= "from Perlito__Python__Prelude import *\n";
    $result .= "import __builtin__\n";
    $result .= "__all__ = []\n";
    $result .= "\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_python() . "\n";
    }
    if ( $execute || $compile_to_bin ) {
        open( OUT, '>', $tmp_filename . '.py' )
          or die "Cannot write to ${tmp_filename}.py\n";
        print OUT $result, "\n";
        close(OUT);
        local $ENV{PYTHONPATH} = 'libpy';
        exec( @cmd, "$tmp_filename.py", @args )
            or die "can't execute";
    }
}
elsif ( $backend eq 'lisp' ) {
    require Perlito::Lisp::Emitter;
    $result .=  ";; Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    $result .= CompUnit::emit_lisp_program( \@comp_unit );

    if ( $execute || $compile_to_bin ) {
        open( OUT, '>', $tmp_filename . '.lisp' )
          or die "Cannot write to ${tmp_filename}.lisp\n";

        my $filename = "lib/Perlito/Lisp/Runtime.lisp";
        warn "reading lisp runtime: $filename\n" if $verbose;
        open FILE, $filename
          or die "Cannot read Lisp runtime: $filename";
        local $/ = undef;
        my $lib = <FILE>;
        print OUT $lib, "\n";

        print OUT $result, "\n";

        if ( $compile_to_bin ) {
            print OUT '(sb-ext:save-lisp-and-die "' . $tmp_filename . '.out" :toplevel \'compiler-main :executable t )' . "\n";
        }
        else {
            print OUT "(compiler-main)\n";
        }

        close(OUT);

        warn "calling lisp compiler\n" if $verbose;
        exec( "sbcl", "--script", "$tmp_filename.lisp", @args )
            or die "can't execute";
    }
}
elsif ( $backend eq 'parrot' ) {
    require Perlito::Parrot::Emitter;
    $result .=  "# Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_parrot() . "\n";
    }

    if ( $execute ) {
        open( OUT, '>', $tmp_filename . '.pir' )
          or die "Cannot write to ${tmp_filename}.pir\n";
        print OUT $result;
        close(OUT);
        warn "calling parrot compiler\n" if $verbose;
        exec( "parrot", "$tmp_filename.pir", @args )
            or die "can't execute";
    }
}
elsif ( $backend eq 'js' ) {
    require Perlito::Javascript::Emitter;
    $result .=  "// Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    for my $p ( @comp_unit ) {
        $result .=  $p->emit_javascript() . "\n";
    }

    if ( $execute ) {
        open( OUT, '>', $tmp_filename . '.js' )
          or die "Cannot write to ${tmp_filename}.js\n";
        my $inc;

        for my $lib_source_filename ( 'lib/Perlito/Javascript/Runtime.js' ) {
            $inc .= "// include file: $lib_source_filename\n";
            open FILE, $::Bin . '/' . $lib_source_filename
              or die "Cannot read $::Bin/$lib_source_filename\n";
            local $/ = undef;
            $inc .= <FILE>;
            close FILE;
            $inc .= "\n// end include file: $lib_source_filename\n";
        }

        print OUT $inc, $result;
        close(OUT);
        my @extra_args;
        if ( $cmd[0] eq 'v8' && @args ) {
            @extra_args = ("--") 
        }
        my @exec = ( @cmd, "$tmp_filename.js", @extra_args, @args );
        warn "calling javascript compiler: @exec\n" if $verbose;
        exec( @exec )
            or die "can't execute";
    }
}
elsif ( $backend eq 'go' ) {
    require Perlito::Go::Emitter;
    $result .=  "// Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";

    my $lib_source_filename = 'lib/Perlito/Go/Runtime.go';
    $result .= "// include file: $lib_source_filename\n";
    open FILE, $::Bin . '/' . $lib_source_filename
      or die "Cannot read $::Bin/$lib_source_filename\n";
    local $/ = undef;
    $result .= <FILE>;
    close FILE;
    $result .= "// end include file: $lib_source_filename\n";

    $result .= CompUnit::emit_go_program( \@comp_unit );

    if ( $execute || $compile_to_bin ) {
        open( OUT, '>', $tmp_filename . '.go' )
          or die "Cannot write to ${tmp_filename}.go\n";
        print OUT $result;
        close(OUT);
        unlink $tmp_filename . '.6';
        unlink '6.out';
        warn "calling go compiler\n" if $verbose;
        my $result = `6g $tmp_filename.go`;
        warn "go compiler: $result\n" if $result; # $verbose && $result;
        $result = `6l $tmp_filename.6`;
        warn "go linker: $result\n" if $result; # $verbose && $result;
    }
    if ( $execute ) {
        warn "now executing\n" if $verbose;
        exec("./6.out", @args)
            or die "can't execute";
    }
}
elsif ( $backend eq 'perl5' ) {
    $result .=  "# Do not edit this file - Generated by $::_V6_COMPILER_NAME $::_V6_COMPILER_VERSION\n";
    $result .= CompUnit::emit_perl5_program( \@comp_unit );

    if ( $execute ) {
        local @ARGV = @args;
        eval $result;
        warn $@ if $@;
    }
}
elsif ( $backend eq 'ast-perl6' ) {
    $result .=  Main::perl( \@comp_unit ) . "\n";
}
elsif ( $backend eq 'ast-perl5' ) {
    require Data::Dumper;
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Indent   = 1;
    $result .=  Data::Dumper::Dumper( \@comp_unit );
}
elsif ( $backend eq 'ast-json' ) {
    require JSON;
    *UNIVERSAL::TO_JSON = sub {
        return { 'bless' => ref($_[0]), %{ $_[0] } };
    };
    $result .= JSON->new->allow_blessed->convert_blessed->encode( \@comp_unit ) . "\n";
}
else {
    die "it seems backend '$backend' is not supported";
}

if ( !$execute && !$compile_to_bin ) {
    print $result;
}

warn "done\n" if $verbose;


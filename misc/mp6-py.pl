package main;

# Usage: perl ./run-mp6.pl [<file>]
# If <file> is not given it will read from stdin.

use strict;
use FindBin '$Bin';
use lib ("$Bin/lib5");

BEGIN {
    $::_V6_COMPILER_NAME    = 'Perlito';
    $::_V6_COMPILER_VERSION = '0.003';
}

use Perlito::Python::Match;

package Main;
use Perlito::Grammar;
use Perlito::Python::Emitter;
use Perlito::Grammar::Regex;
use Perlito::Emitter::Token;

my $source = join('', <> );
my $pos = 0;

# Kludge - make an implicit Main explicit.
$source = "class Main { $source }" if $source !~ /class/;
# Kludge - remove "use v6-alpha;", which mp6 doesn't understand.
$source =~ s/\buse\s*v6-alpha;//;

my $perl5code = "";
$perl5code .= "import MiniPerl.Runtime\n\n";

use Data::Dumper;
sub perl {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad      = '  ';
    local $Data::Dumper::Indent   = 1;
    return Data::Dumper::Dumper( $_[0] );
}

while ( $pos < length( $source ) ) {
    my $p = Perlito::Grammar->comp_unit($source, $pos);
    $perl5code .= perl( $$p ); # join( ";\n", (map { $_->emit() } ($$p) ));
    $perl5code .= ";";
    $pos = $p->to;
}

#$perl5code .= "1;";

#eval($perl5code);
print $perl5code;


package main;

# Usage: perl ./run-mp6.pl [<file>]
# If <file> is not given it will read from stdin.

use strict;
use FindBin '$Bin';
use lib ("$Bin/lib5");

BEGIN {
    $::_V6_COMPILER_NAME    = 'MiniPerl6';
    $::_V6_COMPILER_VERSION = '0.003';
}

use MiniPerl6::Perl5::Runtime;
use MiniPerl6::Perl5::Match;

package Main;
use MiniPerl6::Grammar;
use MiniPerl6::Perl5::Emitter;
use MiniPerl6::Grammar::Regex;
use MiniPerl6::Emitter::Token;

my $source = join('', <> );
my $pos = 0;

# Kludge - make an implicit Main explicit.
$source = "class Main { $source }" if $source !~ /class/;
# Kludge - remove "use v6-alpha;", which mp6 doesn't understand.
$source =~ s/\buse\s*v6-alpha;//;

my $perl5code = "";

use Data::Dumper;
sub perl {
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;
    local $Data::Dumper::Pad      = '  ';
    local $Data::Dumper::Indent   = 1;
    return Data::Dumper::Dumper( $_[0] );
}

while ( $pos < length( $source ) ) {
    my $p = MiniPerl6::Grammar->comp_unit($source, $pos);
    $perl5code .= perl( $$p ); 
    $perl5code .= ";";
    $pos = $p->to;
}

print $perl5code;


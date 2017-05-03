
# this is called from make_jar.sh

use strict;

my @header = <DATA>;;
my $class;

while (<>) {
    my $s = $_;

    if ( 1 .. /^(?:public\s+)?class/ ) {
        next if /^package/;
        push @header, $s unless /^(?:public\s+)?class/;
    }

    if (/^(?:public\s+)?class\s+(\S+)/) {
        $class = $1;
        open F, ">", "org/perlito/Perlito5/$class.java";
        print F @header;
        $s = "public $s" if $s !~ /^public/;
    }
    print F $s if $class;
}

__DATA__
package org.perlito.Perlito5;

import org.perlito.Perlito5.PerlCompare;
import org.perlito.Perlito5.PerlOp;
import org.perlito.Perlito5.PerlRange;
import org.perlito.Perlito5.PerlRange0;
import org.perlito.Perlito5.PerlRangeInt;
import org.perlito.Perlito5.PerlRangeString;
import org.perlito.Perlito5.PerlRangeString1;
import org.perlito.Perlito5.PlArray;
import org.perlito.Perlito5.PlArrayRef;
import org.perlito.Perlito5.PlBool;
import org.perlito.Perlito5.PlCORE;
import org.perlito.Perlito5.PlClass;
import org.perlito.Perlito5.PlClosure;
import org.perlito.Perlito5.PlControlException;
import org.perlito.Perlito5.PlCrypt;
import org.perlito.Perlito5.PlCx;
import org.perlito.Perlito5.PlDieException;
import org.perlito.Perlito5.PlDouble;
import org.perlito.Perlito5.PlFileHandle;
import org.perlito.Perlito5.PlGlobRef;
import org.perlito.Perlito5.PlHash;
import org.perlito.Perlito5.PlHashRef;
import org.perlito.Perlito5.PlInt;
import org.perlito.Perlito5.PlLastException;
import org.perlito.Perlito5.PlLazyIndex;
import org.perlito.Perlito5.PlLazyLookup;
import org.perlito.Perlito5.PlLazyLvalue;
import org.perlito.Perlito5.PlLazyScalarref;
import org.perlito.Perlito5.PlLvalue;
import org.perlito.Perlito5.PlLvalueRef;
import org.perlito.Perlito5.PlNextException;
import org.perlito.Perlito5.PlObject;
import org.perlito.Perlito5.PlROvalue;
import org.perlito.Perlito5.PlRedoException;
import org.perlito.Perlito5.PlReference;
import org.perlito.Perlito5.PlRegex;
import org.perlito.Perlito5.PlRegexResult;
import org.perlito.Perlito5.PlReturnException;
import org.perlito.Perlito5.PlSlice;
import org.perlito.Perlito5.PlString;
import org.perlito.Perlito5.PlStringInputStream;
import org.perlito.Perlito5.PlTieArray;
import org.perlito.Perlito5.PlTieHash;
import org.perlito.Perlito5.PlTieScalar;
import org.perlito.Perlito5.PlUndef;
import org.perlito.Perlito5.PlV;


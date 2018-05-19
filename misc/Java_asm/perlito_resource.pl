
# java -jar perlito5.jar -I src5/lib misc/Java_asm/perlito_resource.pl

# - reads a file from perlito5.jar
#
# - this can be used to point PERL5LIB to the jar file
#   to read the standard Perl lib

use strict;

package P::Object           { import => "org.perlito.Perlito5.PlObject" }
package BufferedReader      { import => "java.io.BufferedReader" }
package InputStreamReader   { import => "java.io.InputStreamReader" }
package InputStream         { import => "java.io.InputStream" }
package StandardCharsets    { import => "java.nio.charset.StandardCharsets" }

my P::Object $pp = P::Object->new();    # indirectly get access to the internal ClassLoader

my BufferedReader $in = BufferedReader->new(
    InputStreamReader->new(
        $pp->getClass()->getResourceAsStream("/META-INF/MANIFEST.MF"),
        StandardCharsets->UTF_8,
    ),
);

# TODO - test defined() in the loop

while (my $s = $in->readLine()) {
    print $s, "\n";
}


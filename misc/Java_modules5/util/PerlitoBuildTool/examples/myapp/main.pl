use strict;

######################
# Some basic examples:

package header { java_path => 'org.perlito.myapp' } # <- package definition (all classes go to this package)

package Date   { import    => "java.util.Date"    } # <- import java package

package int    {                                  } # <- primitive java type

##################################################
# Custom perl modules go in the same file, for now
package My::Module {
    sub do_stuff {
        say "Count 400x400x400";

        my int $count = 0;
        my int $i = 0;
        while ( $i < 400 ) {
            my int $j = 0;
            while ( $j < 400 ) {
                my int $k = 0;
                while ( $k < 400 ) {
                    $k = $k + 1;
                    $count = $count + 1;
                }
                $j = $j + 1;
            }
            $i = $i + 1;
        }
        my $pCount = $count; # cast java int to perl scalar
                             # since we dont have string
                             # interpolation for non-perl types
        print "done count => $pCount\n";
    }
}

######
# Main
My::Module->do_stuff();


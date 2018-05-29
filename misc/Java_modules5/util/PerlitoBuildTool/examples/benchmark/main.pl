use strict;

use feature 'say';

######################
# Some basic examples:
#
package header  { java_path => 'org.perlito.benchmark' } # <- package definition (all classes go to this package)

package int     {                                      } # <- primitive java type

##################################################
# Custom perl modules go in the same file, for now
#
package My::Module {

    use Data::Dumper;
    
    sub do_stuff {

        say "Loop 400x400x400";

        my int $count = 0;
        my int $i = 0;

        while ($i < 400) {

            my $h = {};

            my $j = 0;

            while ( $j < 400 ) {

                $h->{$j} = {};

                # cant use native int type if its going to be a hash key
                my $k = 0;
                while ( $k < 400 ) {
                    $k = $k + 1;
                    $count = $count + 1;

                    $h->{$j}->{$k}++;
                }
                $j = $j + 1;
            }


            # misc array, hash and regex operations, trying to aproximate
            # a mixed type workload
            my @a = map { $_ * 2 } keys %$h;

            map {
                my $new_value = ["a".."z"]->[rand($_ % 26)] x $_;
                $h->{$_} = $new_value
            } keys %$h;

            map { ($_ && $h->{$_}) ? 1 : 0 } @a;

            if (scalar grep { $_=~/aaa/g } grep { length $_ > 5 } values %$h) {
                my $y = 1;    
            }
            
            # print Dumper($h);
            
            $i = $i + 1;
        }

        # cast java int to perl scalar since we dont have string
        # interpolation for non-perl types. 
        my $pCount = $count; 

        # now we can use it as regula perl variable in string
        # context
        print "done count => $pCount\n";

    }
}

######
# Main
my $t = time;
My::Module->do_stuff();
my $e = time;

say "duration => " . ($e - $t) . "\n";


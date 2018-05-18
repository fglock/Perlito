
use v5;
use feature 'say';
use strict;
package Calendar { import => "java.util.Calendar" }
package TimeZone { import => "java.util.TimeZone" }

say "1..1";

# this Perl example was translated from this Java source code:
# http://www.java2s.com/Tutorial/Java/0120__Development/TimeZonegetTimeZoneAmericaNewYork.htm

my Calendar $calNewYork = Calendar->getInstance();

$calNewYork->setTimeZone(TimeZone->getTimeZone("America/New_York"));

my $hour = $calNewYork->get(Calendar->HOUR_OF_DAY);
my $min  = $calNewYork->get(Calendar->MINUTE);
printf "# Time in New York: %02d:%02d\n", $hour, $min;

if (length($hour) < 1) {
    print "not ";
}
say "ok 1 # $hour";

say "# done";


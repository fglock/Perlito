# this Perl example was translated from this Java source code:
# http://www.java2s.com/Tutorial/Java/0120__Development/TimeZonegetTimeZoneAmericaNewYork.htm
#
# usage:
#
# $ perl perlito5.pl -Isrc5/lib -I. -It -Cjava misc/Java/Calendar.pl > Main.java ; javac Main.java ; java Main
# Time in New York: 7:30
#

use feature 'say';
use strict;
package Calendar { import => "java.util.Calendar" }
package TimeZone { import => "java.util.TimeZone" }

my Calendar $calNewYork = Calendar->getInstance();

$calNewYork->setTimeZone(TimeZone->getTimeZone("America/New_York"));

my $hour = $calNewYork->get(Calendar->HOUR_OF_DAY);
my $min  = $calNewYork->get(Calendar->MINUTE);
printf "Time in New York: %02d:%02d\n", $hour, $min;


# this Perl example was translated from this Java source code:
# http://www.java2s.com/Tutorial/Java/0120__Development/TimeZonegetTimeZoneAmericaNewYork.htm
#
# usage:
#
# $ java -jar perlito5.jar -Isrc5/lib -I. -It misc/Java/CalendarEval.pl
# Time in New York: 7:30
#

my ($hour, $min);

eval <<'EOT';
    use feature 'say';
    use strict;
    package Calendar { import => "java.util.Calendar" }
    package TimeZone { import => "java.util.TimeZone" }
    
    my Calendar $calNewYork = Calendar->getInstance();
    
    $calNewYork->setTimeZone(TimeZone->getTimeZone("America/New_York"));
    
    $hour = $calNewYork->get(Calendar->HOUR_OF_DAY);
    $min  = $calNewYork->get(Calendar->MINUTE);
EOT

printf "Time in New York: %02d:%02d\n", $hour, $min;


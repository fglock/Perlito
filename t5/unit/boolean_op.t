use feature 'say';

say '1..3';

$x;

$x && say 'not ok ';
!$x && say 'ok 1 - negate and and';

$x || say 'ok 2 - or';

$x = 1;
$x && say 'ok 3 - And with true';



use feature 'say';

say '1..9';

say 'ok ' . '1 - concatenated';
say 'ok ' . ( 1 + 1 ) . ' - added ';
say 'ok ' . ( 5 - 2 ) . ' - substracted';
say 'ok ' . ( 2 * 2 ) . ' - multiplied';
say 'ok ' . ( 10 / 2 ). ' - divided ';
say 'ok ' . ( 13 % 7 ) . ' - mod ';

$x = 6;
say 'ok ' . ( ++$x ) . ' - increment';
$x = 9;
say 'ok ' . ( --$x ) . ' - decrement'; 

say 'ok ' . 3 ** 2 . ' - exponential';



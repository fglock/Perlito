
use v5;
use feature 'say';

say "1..1";

Java::inline q{
    System.out.println("ok 1");
};

say "# done";


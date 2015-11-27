use Test::More tests => 3;
BEGIN { use_ok('ReturnArray') }
my @word = ReturnArray::test();
is( $word[0], "Hello" );
is( $word[1], "World" );

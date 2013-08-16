use Test::More tests => 2;
BEGIN { use_ok('ReturnHelloWorld') }
is( ReturnHelloWorld::test(), "Hello World" );

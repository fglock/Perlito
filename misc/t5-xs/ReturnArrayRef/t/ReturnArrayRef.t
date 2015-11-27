use Test::More tests => 2;
BEGIN { use_ok('ReturnArrayRef') }
ok( ReturnArrayRef::test() );

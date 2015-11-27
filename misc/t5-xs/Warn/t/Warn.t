use Test::More tests => 2;
BEGIN { use_ok('Warn') }
ok( Warn::test() );

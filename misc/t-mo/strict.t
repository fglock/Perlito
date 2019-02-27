use Test::More tests => 1;

eval 'package Foo; use Mo; $x = 1';

like $@, qr/Global symbol "\$x" requires explicit package name/,
    'Mo is strict';

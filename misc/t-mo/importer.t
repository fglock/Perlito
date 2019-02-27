use Test::More tests => 1;

{
    package Foo;

    sub importer {
        package main;
        is "@_", 'Foo importer default build', 'Mo::importer works';
    }

    use Mo qw'importer default build';
}

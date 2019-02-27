use Test::More;

no warnings;
\&::main::;

plan tests => 2;

eval 'use Mo';
ok !$@, 'use Mo works with global sub called "" ' . ($@||'');

eval 'Mo->import; main->new';
ok !$@, 'Mo->new works with global sub called "" ' . ($@||'');

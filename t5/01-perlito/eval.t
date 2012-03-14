say '1..2';
eval 'say "ok 1 # say from eval"';
eval '{;say "not ok 1"';
say "ok 2 # we live after evaling incorrect code";

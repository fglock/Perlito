print "1..2\n";
eval 'print "ok 1 # say from eval\\n"';
eval '{;print "not ok 1\\n"';
print "ok 2 # we live after evaling incorrect code\n";

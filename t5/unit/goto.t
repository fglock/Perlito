use feature 'say';

say '1..1';

# goto forward

goto END;

    print "not ok 1\n";

END:
    print "ok 1\n";


# TODO

# # goto forward
# 
# for my $i (1, 2) {
# 
#     goto L1 if $i == 1;
# 
#     print "ok 3\n";
# 
#     goto L2;
# 
#     print "not ok 3\n";
# 
#   L1:
#     print "ok 2\n";
# 
#     goto L2;
# 
#     print "not ok 2\n";
# 
#   L2:
# 
# }
# 

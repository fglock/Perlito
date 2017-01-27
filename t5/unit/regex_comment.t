use v5;
use feature 'say';
use strict;
use warnings;

say '1..4';

my $str = "Perl";

print "not " unless $str =~ /P # comment/x;
say 'ok 1';

print "not " unless $str =~ /P # comment
    erl /x;
say 'ok 2';

print "not " unless $str =~ /P(?# comment )erl/;
say 'ok 3';

print "not " unless $str =~ /P(?# comment \/  )erl/;
say 'ok 4';


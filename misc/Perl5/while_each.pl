# http://blogs.perl.org/users/vkroll/2015/09/how-each-drove-me-crazy.html

use strict;

my @data = qw( bla fasel foo org jawohl hmblamm glfoo sdfoo sadfasffoo) ;

my %regex = ( type1 => 'bla', type2 => 'foo');

sub test_it {
    my $entry = shift;
    while( my($type, $regex) = each %regex) {
        return 1 if $entry =~ /$regex/;
    }
    return 0;
}

foreach(@data) {
    print $_,"\t";
    print " matches " if test_it($_);
    print "\n";
}



#!/usr/bin/perl
# typeglob-name-package.pl

print "1..6\n";

package X::Y;

$foo = "Some value";
$bar = "Another value";

my $f = \*xyz;
my $g = *xyz;

who_am_i( *foo, 1, "X::Y", "foo" );
who_am_i( $f,   3, "X::Y", "xyz" );
who_am_i( $g,   5, "X::Y", "xyz" );

sub who_am_i {
    my ( $glob, $num, $pkg, $name ) = @_;

    print "not " if *{$glob}{PACKAGE} ne $pkg;
    print "ok $num # ", *{$glob}{PACKAGE} . "\n";

    print "not " if *{$glob}{NAME} ne $name;
    print "ok ", ($num + 1) , " # ", *{$glob}{NAME} . "\n";

}

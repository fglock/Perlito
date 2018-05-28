#!/usr/bin/perl
# typeglob-name-package.pl

print "1..18\n";

package X::Y;

$foo = "Some value";
$bar = "Another value";

my $f = \*xyz;
my $g = *xyz;

who_am_i( *foo,     1, "X::Y", "foo", "" );
who_am_i( \*foo,    4, "X::Y", "foo", "GLOB" );
who_am_i( *{"foo"}, 7, "X::Y", "foo", "" );
who_am_i( $f,      10, "X::Y", "xyz", "GLOB" );
who_am_i( $g,      13, "X::Y", "xyz", "" );
who_am_i( \$g,     16, "X::Y", "xyz", "GLOB" );
# who_am_i( \$f,     16, "X::Y", "xyz", "REF" );

sub who_am_i {
    my ( $glob, $num, $pkg, $name, $ref ) = @_;

    print "not " if *{$glob}{PACKAGE} ne $pkg;
    print "ok $num # ", *{$glob}{PACKAGE} . "\n";

    print "not " if *{$glob}{NAME} ne $name;
    print "ok ", ($num + 1) , " # ", *{$glob}{NAME} . "\n";

    print "not " if ref($glob) ne $ref;
    print "ok ", ($num + 2) , " # ", ref($glob) . "\n";

}

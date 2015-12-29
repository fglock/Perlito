package J::Date   { import => "java.util.Date" };
package J::String { import => "java.lang.String" };

package My::Date {
    extends => 'J::Date',
    methods => [
        toString => {
            decl => [ "public", "J::String" ],
            args => [],
            code => "main::date_string",
        },
    ],
}

package main;

sub date_string {
    my $s = J::String->new("Hello");
    return $s;
}

my J::Date $j_date = J::Date->new();
my $s1 = $j_date->toString();
my My::Date $date = My::Date->new();
my J::String $j_str = $date->toString();
my $s2 = $j_str->toString();

print $s1, " ", $s2, "\n";   # date / hello

1;

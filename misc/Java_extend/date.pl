package J::Date   { import => "java.util.Date" };
# package J::String { import => "java.lang.String" };

package My::Date {
    extends => 'J::Date',
    methods => [
        toString => {
            decl => [ "public", "String" ],
            args => [],
            code => "main::date_string",
        },
    ],
}

package main;

sub date_string {
    return "Hello";
}

my J::Date $j_date = J::Date->new();
my $s1 = $j_date->toString();
my My::Date $date = My::Date->new();
my $s2 = $date->toString();

print $s1, " ", $s2, "\n";   # date / hello

1;

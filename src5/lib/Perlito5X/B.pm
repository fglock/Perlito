package B;

use Data::Dumper;  # escape_string

sub import {
    my $class = shift;
    my $caller = caller(0);

    if (grep { $_ eq 'perlstring' } @_) {
        *{"${caller}::perlstring"} = \&perlstring;
    }
}

sub perlstring {
    return Data::Dumper::escape_string(@_);
}

1;


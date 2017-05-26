use v5;
use strict;
use feature 'say';

# use Data::Dumper;

{

    package TheHash;

    my $debug = 0;

    sub TIEHASH {
        my $storage = bless {}, shift;
        warn "# New ReportHash created, stored in $storage.\n" if $debug;
        $storage;
    }

    sub STORE {
        warn "# STORE data with key $_[1] = $_[2].\n" if $debug;
        $_[0]{ $_[1] } = $_[2];
    }

    sub FETCH {
        warn "# FETCH data with key $_[1] = $_[0]{ $_[1] }.\n" if $debug;
        $_[0]{ $_[1] };
    }

    sub FIRSTKEY {
        warn "# FIRSTKEY\n" if $debug;
        my $a = scalar keys %{ $_[0] };
        each %{ $_[0] };
    }

    sub NEXTKEY {
        warn "# NEXTKEY\n" if $debug;
        each %{ $_[0] };
    }

    # sub dump {
    #     print STDERR Data::Dumper::Dumper($_[0]);
    # }

}

say '1..2';

my %hash;


sub ti {
    tie %{$_[0]}, 'TheHash';
}

ti(\%hash);

print 'not ' if ! tied(%hash);
say "ok 1 - tied # ", tied(%hash);

$hash{abc} = 3;
$hash{xyz} = 4;

my @out;
while ( my ( $key, $value ) = each %hash ) {
    push @out, ( $key, $value );
}

print 'not ' if "@out" ne "abc 3 xyz 4" && "@out" ne "xyz 4 abc 3";
say "ok 2 - while-each # @out";



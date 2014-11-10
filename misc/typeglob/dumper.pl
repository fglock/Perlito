
use Data::Dump::Streamer;

BEGIN {
    # the code to be dumped:
    do "perlito5.pl";
}

sub dumpme {
    no strict 'refs';

    my %out;
    my %done;
    my @todo = 'main::';
    # my @todo = 'Perlito5::';

    $done{"Data::Dump::Streamer::"} = 1;
    $done{"DynaLoader::"} = 1;
    $done{"Config::"} = 1;

    while (@todo) {
        my $namespace = shift(@todo);
        next if $done{$namespace};
        print "#\n";
        foreach my $entry ( keys %{$namespace} ) {
            if ( length($entry) > 2 && substr( $entry, -2, 2 ) eq '::' ) {
                if ( $namespace eq 'main::' ) {
                    push @todo, $entry;
                }
                else {
                    push @todo, $namespace . $entry;
                }
                next;
            }
            print "# $namespace $entry\n";
            local *g = ${$namespace}{$entry};
            if ( defined *g{SCALAR} ) { $out{$namespace}{$entry}{SCALAR} = *g{SCALAR} }
            if ( defined *g{ARRAY} )  { $out{$namespace}{$entry}{ARRAY}  = *g{ARRAY} }
            if ( defined *g{HASH} )   { $out{$namespace}{$entry}{HASH}   = *g{HASH} }
            if ( defined *g{CODE} )   { $out{$namespace}{$entry}{CODE}   = *g{CODE} }
        }
        $done{$namespace} = 1;
    }
    for (keys %out) {
        print "# [[ $_\n";
        print Dump( $out{$_} ), "\n";
        print "# $_ ]]\n\n";
    }
}

BEGIN {
    dumpme();
    exit(0);    # don't execute any further
}


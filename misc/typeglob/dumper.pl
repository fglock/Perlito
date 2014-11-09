use Data::Dump::Streamer;

{
    no strict 'refs';

    my %out;
    my %done;
    my @todo = 'main::';

    while (@todo) {
        my $namespace = shift(@todo);
        next if $done{$namespace};
        foreach my $entry ( keys %{$namespace} ) {
            if ( length($entry) > 2 && substr( $entry, -2, 2 ) eq '::' ) {
                push @todo, $entry;
            }
            local *g = $Exporter::{$entry};
            if ( defined *g{SCALAR} ) { $out{$namespace}{$entry}{SCALAR} = *g{SCALAR} }
            if ( defined *g{ARRAY} )  { $out{$namespace}{$entry}{ARRAY}  = *g{ARRAY} }
            if ( defined *g{HASH} )   { $out{$namespace}{$entry}{HASH}   = *g{HASH} }
            if ( defined *g{CODE} )   { $out{$namespace}{$entry}{CODE}   = *g{CODE} }
        }
        $done{$namespace} = 1;

    }
    print Dump( \%out ), "\n";
}


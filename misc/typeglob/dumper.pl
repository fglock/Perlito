use Data::Dump::Streamer;

{
    no strict 'refs';

    my %out;

    foreach my $entry ( keys %Exporter:: ) {
    # foreach my $entry ( keys %main:: ) {
        local *g = $Exporter::{$entry};
        if ( defined *g{SCALAR} ) { $out{$entry}{SCALAR} = *g{SCALAR} }
        if ( defined *g{ARRAY} )  { $out{$entry}{ARRAY}  = *g{ARRAY} }
        if ( defined *g{HASH} )   { $out{$entry}{HASH}   = *g{HASH} }
        if ( defined *g{CODE} )   { $out{$entry}{CODE}   = *g{CODE} }
    }
    print Dump( \%out ), "\n";
}


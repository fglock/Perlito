use Data::Dump::Streamer;

{
    no strict 'refs';

    foreach my $entry ( keys %main:: ) {
        print "$entry\n";
    }
    print Dump( \*Exporter );
    foreach my $entry ( keys %Exporter:: ) {
        print "$entry ", Dump($Exporter::{$entry}), "\n";
    }
}

my $v = {
    v => 1,
    d => \&Dump,
};

print Dump($v);


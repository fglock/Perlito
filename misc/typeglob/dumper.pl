use Data::Dump::Streamer;

{
    no strict 'refs';

    foreach my $entry ( keys %main:: ) {
        print "$entry\n";
        print "\tscalar is defined\n" if defined ${$entry};
        print "\tarray  is defined\n" if defined @{$entry};
        print "\thash   is defined\n" if defined %{$entry};
        print "\tsub    is defined\n" if defined &{$entry};
    }
    print Dump( \*Exporter );
    foreach my $entry ( keys %Exporter:: ) {
        print "$entry ", Dump($Exporter::{$entry}), "\n";
        print "\tscalar is defined\n" if defined ${$entry};
        print "\tarray  is defined\n" if defined @{$entry};
        print "\thash   is defined\n" if defined %{$entry};
        print "\tsub    is defined\n" if defined &{$entry};
    }
}

my $v = {
    v => 1,
    d => \&Dump,
};

print Dump($v);



use Data::Dump::Streamer;

BEGIN {
    # the code to be dumped:
    do "perlito5.pl";
}

BEGIN {
    no strict 'refs';

    my %out;
    my %done;
    my @todo = 'main::';

    $done{"Data::Dump::Streamer::"} = 1;
    $done{"DynaLoader::"} = 1;
    $done{"Config::"} = 1;
    $done{"B::"} = 1;

    while (@todo) {
        my $namespace = shift(@todo);
        next if $done{$namespace};
        # print "#\n";
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
            # print "# $namespace $entry\n";
            local *g = ${$namespace}{$entry};
            if ( defined *g{SCALAR} ) { $out{$namespace}{$entry}{SCALAR} = *g{SCALAR} }
            if ( defined *g{ARRAY} )  { $out{$namespace}{$entry}{ARRAY}  = *g{ARRAY} }
            if ( defined *g{HASH} )   { $out{$namespace}{$entry}{HASH}   = *g{HASH} }
            if ( defined *g{CODE} )   { $out{$namespace}{$entry}{CODE}   = *g{CODE} }
        }
        $done{$namespace} = 1;
    }
    for (keys %out) {
        my $package = $_;
        $package =~ s/::$//;
        print "# start: $_\n";
        print "{\n";
        print "    package $package;\n";
        print "    no strict ('vars');\n";
        print "    our \$HASH1;\n";

        my $dump = Dump( $out{$_} )->Out;
        $dump =~ s/\$main:::/\$main::{':'}/g; # $:
        print "    $dump\n";
        # print Dump( $out{$_} ), "\n";

        print '    for my $name (keys %$HASH1) {' . "\n";
        print '        my $g = $HASH1->{$name};' . "\n";
        print '        if (exists $g->{SCALAR}) { ${$name} = ${$g->{SCALAR}} };' . "\n";
        print '        if (exists $g->{ARRAY})  { *{$name} = $g->{ARRAY} };' . "\n";
        print '        if (exists $g->{HASH})   { *{$name} = $g->{HASH} };' . "\n";
        print '        if (exists $g->{CODE})   { *{$name} = $g->{CODE} };' . "\n";
        print "    }\n";
        print "}\n";
        print "# end: $_\n\n";
    }

    exit(0);    # don't execute any further
}

=pod

=head1 NAME

dumper - a Perl5 preprocessor

=head1 SYNOPSIS

    dumper

=head1 DESCRIPTION

This program reads Perl5 source code and generates a runnable
representation of the "typeglob tree".

=head1 AUTHORS

Flavio Soibelmann Glock <fglock@gmail.com>.

=head1 SEE ALSO

L<http://fglock.github.io/Perlito>

=head1 COPYRIGHT

Copyright 2014 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut


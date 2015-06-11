package Data::Dumper;
use Perlito5::Dumper;

sub import {
    my $pkg     = shift;
    my $callpkg = caller(0);
    *{ $callpkg . "::Dumper" } = \&Dumper;
    return;
}

sub Dumper {
    my $seen  = {};
    my $level = '    ';
    my @out;
    for my $i (0 .. $#_) {
        my $pos   = '$VAR' . ($i + 1);
        push @out, "$pos = " . Perlito5::Dumper::_dumper($_[$i], $level, $seen, $pos) . ";\n";
    }
    return join('', @out);
}

1;


use Data::Dumper;
use strict;

# this is the algorithm for keeping the compile-time environment in pure-perl
# incrementally set environment; and keep a pad stack


my @prog = (

    q!  my $x = 3       !,

    q!  print "x=$x\n"  !,

    q!  my $y = 4;      !,

    q!  print "y=$y\n"  !,

    q!  my $z = 7;      !,

    q!  $y++            !,

    q!  print "y=$y\n"  !,

    q!  my $k = $y + 1; print "k=", $k, "\n"  !,

);

my @vars;
sub get_vars {
    my $s = shift;
    push @vars, $s =~ /([\$\@\%]\w+)/;
}

my $pad;
while (@prog) {

    my $line = shift(@prog);
    get_vars($line);
    my $cmd =
          'do { '
        .   $line . '; '
        .   'sub { '
        .       join(',', @vars) . '; '
        .       'eval $_[0] '
        .   '} '
        . '}';

    if ($pad) {
        $pad = $pad->($cmd)
    }
    else {
        $pad = eval $cmd;
    }

    #print "cmd $cmd\n";
    #print "pad $pad - $@\n";

}

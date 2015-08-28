use strict;
use warnings;

# run with cat Test.java | grep class | perl depgraph.pl > deps.svg

use Data::Dumper;
use Graph::Easy;

# get from STDIN
my @classes = map {
    chomp; [grep { $_ ne "class" && $_ ne "{" and $_ ne "" } split(/\s/, $_)]
} (<>);

my $graph = Graph::Easy->new();

my %tree = ();
my %nodes = ();

for my $cl (@classes) {
    if (scalar @$cl == 1) {
        $tree{$cl->[0]} = undef;
        $graph->add_node($cl->[0]);
    } else {
        $nodes{$cl->[0]} = $cl->[2] 
    } 
}

my @nodes = map { { name => $_, extends => $nodes{$_} } } keys %nodes;

add_to_tree(\%tree, \@nodes);

# print Dumper(\%tree);

print $graph->as_svg();

sub add_to_tree {
    my $level = shift;
    my $nodes = shift;

    foreach my $class (keys %$level) {
        foreach my $node (@{[grep { ! $_->{assigned} } @nodes]}) {
            if ($class eq $node->{extends}) {
                $level->{$class}->{$node->{name}} = undef;
                $graph->add_node($node->{name});
                $graph->add_edge($class, $node->{name});
                $node->{assigned} = 1;
            }
            else {
                if ($level->{$class}) { # not leaf
                    my $next_level = $level->{$class}; # hash of child nodes
                    add_to_tree($next_level, $nodes);
                }
            }
        }
    }
}

use strict;
use warnings;

# Subroutine to process Perl prototypes
sub process_prototype {
    my ($prototype, @args) = @_;
    my @protos = split //, $prototype;

    die "Argument count mismatch\n" if @protos > @args;

    my %processed_args;
    for my $i (0 .. $#protos) {
        my $proto = $protos[$i];
        my $arg   = $args[$i];

        if ($proto eq '$') {
            die "Expected scalar, got " . ref($arg) . "\n" if ref($arg);
            $processed_args{"arg$i"} = $arg;
        }
        elsif ($proto eq '@') {
            die "Expected array, got " . ref($arg) . "\n" unless ref($arg) eq 'ARRAY';
            $processed_args{"arg$i"} = $arg;
        }
        elsif ($proto eq '%') {
            die "Expected hash, got " . ref($arg) . "\n" unless ref($arg) eq 'HASH';
            $processed_args{"arg$i"} = $arg;
        }
        elsif ($proto eq '*') {
            die "Expected glob, got " . ref($arg) . "\n" unless ref($arg) eq 'GLOB';
            $processed_args{"arg$i"} = $arg;
        }
        elsif ($proto eq '\\') {
            my $next_proto = $protos[++$i];
            if ($next_proto eq '@') {
                die "Expected array reference, got " . ref($arg) . "\n" unless ref($arg) eq 'ARRAY';
                $processed_args{"arg$i"} = $arg;
            }
            elsif ($next_proto eq '%') {
                die "Expected hash reference, got " . ref($arg) . "\n" unless ref($arg) eq 'HASH';
                $processed_args{"arg$i"} = $arg;
            }
            elsif ($next_proto eq '$') {
                die "Expected scalar reference, got " . ref($arg) . "\n" unless ref($arg) eq 'SCALAR';
                $processed_args{"arg$i"} = $arg;
            }
            elsif ($next_proto eq '[') {
                my $next_char = $protos[++$i];
                my $end_bracket = $protos[++$i];
                die "Expected closing bracket ]" unless $end_bracket eq ']';
                if ($next_char eq '%') {
                    die "Expected hash reference, got " . ref($arg) . "\n" unless ref($arg) eq 'HASH';
                    $processed_args{"arg$i"} = $arg;
                }
                elsif ($next_char eq '@') {
                    die "Expected array reference, got " . ref($arg) . "\n" unless ref($arg) eq 'ARRAY';
                    $processed_args{"arg$i"} = $arg;
                }
                elsif ($next_char eq '$') {
                    die "Expected scalar reference, got " . ref($arg) . "\n" unless ref($arg) eq 'SCALAR';
                    $processed_args{"arg$i"} = $arg;
                }
                else {
                    die "Unknown prototype element: \\[$next_char]\n";
                }
            }
            else {
                die "Unknown prototype element: \\$next_proto\n";
            }
        }
        else {
            die "Unknown prototype element: $proto\n";
        }
    }

    return \%processed_args;
}

# Example usage
my @array = (1, 2, 3);
my %hash = (key => 'value');
my $scalar = 42;

my $proto1 = '\@$$@';
my @args1  = (\@array, 0, 2, @array);

my $proto2 = '\[%@]';
my @args2  = (\%hash);

my $processed_args1 = process_prototype($proto1, @args1);
my $processed_args2 = process_prototype($proto2, @args2);

use Data::Dumper;
print Dumper($processed_args1);
print Dumper($processed_args2);

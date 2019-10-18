
package JavaRDD { import => "org.apache.spark.api.java.JavaRDD" }
package JavaSparkContext { import => "org.apache.spark.api.java.JavaSparkContext" }
package SparkSession { import => "org.apache.spark.sql.SparkSession" }
package Java::ArrayList { import => "java.util.ArrayList" }
package Java::List { import => "java.util.List" }
package Integer { import => "java.lang.Integer" }
package List::Integer { import => "java.util.ArrayList<java.lang.Integer>" }

use strict;
use warnings;
use Java;

my SparkSession $spark = SparkSession
      ->builder()
      ->appName("JavaSparkPi")
      ->config("spark.master", "local")
      ->getOrCreate();

my JavaSparkContext $jsc = JavaSparkContext->new($spark->sparkContext());

my Integer $slices = $ARGV[0] // 2;
my $n = 100000 * $slices;

print "slices: $slices; n: $n\n";

my List::Integer $list = List::Integer->new();
for ( 0 .. $n - 1 ) {
    my Integer $i = int($_);
    $list->add($i);
}

my $dataSet = $jsc->parallelize($list, $slices);

sub pi_func {
      my $x = rand(2) - 1;
      my $y = rand(2) - 1;
      return ($x * $x + $y * $y <= 1) ? 1 : 0;
}

my $count;

# TODO
# $count = $dataSet->map(
#         ...
#     )->reduce(
#         ...
#     );

print "Pi is roughly ", 4.0 * $count / $n, "\n";

$spark->stop();


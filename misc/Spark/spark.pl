
package JavaRDD { import => "org.apache.spark.api.java.JavaRDD" }
package JavaSparkContext { import => "org.apache.spark.api.java.JavaSparkContext" }
package SparkSession { import => "org.apache.spark.sql.SparkSession" }
package Integer { import => "java.lang.Integer" }
package List { import => "java.util.ArrayList<PlObject>" }

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

my List $list = List->new();
for ( 0 .. $n - 1 ) {
    $list->add($_);
}

my $dataSet = $jsc->parallelize($list, $slices);


my $count;

# TODO
# $count = $dataSet->map(
#         sub ($) {
#               my $x = rand(2) - 1;
#               my $y = rand(2) - 1;
#               return ($x * $x + $y * $y <= 1) ? 1 : 0;
#         }
#     )->reduce(
#         sub ($$) { $_[0] + $_[1] }
#     );

print "Pi is roughly ", 4.0 * $count / $n, "\n";

$spark->stop();


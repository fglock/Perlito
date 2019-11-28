
package JavaRDD::String { import => "org.apache.spark.api.java.JavaRDD<String>" }

package JavaSparkContext { import => "org.apache.spark.api.java.JavaSparkContext" }

package SparkSession { import => "org.apache.spark.sql.SparkSession" }

package Integer { import => "java.lang.Integer" }

package List { import => "java.util.ArrayList<String>" }

use strict;
use warnings;
use Java;

my SparkSession $spark =
  SparkSession
  ->builder()
  ->appName("JavaSparkPi")
  ->config( "spark.master", "local" )
  # ->config( "spark.jars", "./perlito5.jar,./spark-2.4.4-bin-hadoop2.7/jars/*" )
  ->getOrCreate();

my JavaSparkContext $sc = JavaSparkContext->new( $spark->sparkContext() );

my JavaRDD::String $dataSet = $sc->textFile( "test.txt" );

my $v = $dataSet->first();
print "got [$v]\n";

$spark->stop();


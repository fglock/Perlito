package MR::TableMapper {
  import    => "TableMapper",
  java_type => "TableMapper<ImmutableBytesWritable, Put>"
}

# ... others

package Put {
  import => 'org.apache.hadoop.hbase.Put'
}

package Reducer {
  import => 'org.apache...Reducer'
}

package Boolean {
  import 'java.lang.Boolean';
}

# Impl
package MyMapper;
use JMoose;

extends 'MR::TableMapper';
method 'map' => (
  is   => ['public', 'void'],
  in   => ['ImmutableBytesWritable row', 'Result value', 'Context context'],
  out  => undef,
  code => sub {
    my ($row, $value $context) = @_;
    # ...
  },
);
method 'resultToPut' => (
  is  => ['private', 'static'],
  in  => ['ImmutableBytesWritable key', 'Result result']
  out => 'Put',
  code => sub {
    my ($key, $result) = @_;
    # ...
  }
);

package MyReducer;
use JMoose;

extends 'Reducer';
method 'reduce' => (
  is => ['public', 'void'],
  in => ['Text key', 'Iterable<IntWritable> values', 'Context context'],
  out => undef,
  code => sub {
      my ($key, $values, $context) = @_;
      # ...
  }
);

package main;

my Configuration $config = HBaseConfiguration->create();
my Job $job = Job->new($config,"ExampleSummaryToFile");

$job->setJarByClass(MySummaryFileJob->class); # class that contains mapper and reducer

my Scan $scan = Scan->new();
$scan->setCaching(500);        # 1 is the default in Scan, which will be bad for MapReduce jobs
$scan->setCacheBlocks(Boolean->false);  # don't set to true for MR jobs

# set other scan attrs

# TODO: create sourceTable
TableMapReduceUtil->initTableMapperJob(
    $sourceTable,        # input table
    $scan,               # Scan instance to control CF and attribute selection
    MyMapper->class,     # mapper class
    Text->class,         # mapper output key
    IntWritable->class,  # mapper output value
    $job
);

$job->setReducerClass(MyReducer->class);    # reducer class
$job->setNumReduceTasks(1);    # at least one, adjust as required
FileOutputFormat->setOutputPath(
    $job,
    Path->new("/tmp/mr/mySummaryFile")
);  # adjust directories as required


my Boolean $b = job->waitForCompletion(Boolean.true);
if (!$b) {
  throw(IOException->new("error with job!"));
}


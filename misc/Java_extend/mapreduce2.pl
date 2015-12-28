package MR::TableMapper {
    import    => "TableMapper",
    java_type => "TableMapper<ImmutableBytesWritable, Put>",
}

# ... others

package Put {
    import => 'org.apache.hadoop.hbase.Put',
}

package Reducer {
    import => 'org.apache...Reducer',
}

package Boolean {
    import => 'java.lang.Boolean',
}

# Impl
package MyMapper {
    extends => 'MR::TableMapper',
    methods => [
        'map' => {
            keywords => [ 'public', 'void' ],
            returns  => 'void',
            code     => sub {
                my ImmutableBytesWritable $row = shift;
                my Result $value               = shift;
                my Context $context            = shift;

                # ...
            },
        },
        'resultToPut' => {
            keywords => [ 'private', 'static' ],
            returns  => 'Put',
            code     => sub {
                my ImmutableBytesWritable $key = shift;
                my Result $result              = shift;

                # ...
            },
        },
    ],
}

package MyReducer {
    extends => 'Reducer',
    methods => [
        'reduce' => {
            keywords => [ 'public', 'void' ],
            returns  => 'void',
            code     => sub {
                # ['Text key', 'Iterable<IntWritable> values', 'Context context'],
                # my ( $key, $values, $context ) = @_;

                # ...
            },
        },
    ],
}

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


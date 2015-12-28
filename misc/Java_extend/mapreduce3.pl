package MR::TableMapper {
    import    => "TableMapper",
    java_type => "TableMapper<ImmutableBytesWritable, Put>",
}

package ImmutableBytesWritable {
    import => 'org...ImmutableBytesWritable',
}

package Put {
    import => 'org.apache.hadoop.hbase.Put',
}

package Result {
    import => 'org.apache...Result',
}

package Context {
    import => 'org.apache...Context',
}

package Text {
    import => 'org.apache...Text',
}

package Iterable::IntWritable {
    import => 'org.apache...Iterable::IntWritable',
}

package Configuration {
    import => 'org.apache...Configuration',
}

package Job {
    import => 'org.apache...Job',
}

package Scan {
    import => 'org.apache...Scan',
}

package Reducer {
    import => 'org.apache...Reducer',
}

package Boolean {
    import => 'java.lang.Boolean',
}

# Impl
package MyMapper {
    extends => 'MR::TableMapper';
    sub MODIFY_CODE_ATTRIBUTES { }

    sub map :public :void {
        my ImmutableBytesWritable $row = shift;
        my Result $value               = shift;
        my Context $context            = shift;

        # ...
        return;
    }

    sub resultToPut :private :static :Put {
        my ImmutableBytesWritable $key = shift;
        my Result $result              = shift;

        # ...
        my Put $put = Put->new();
        return $put;
    }
}

package MyReducer {
    extends => 'Reducer';
    sub MODIFY_CODE_ATTRIBUTES { }

    sub reduce :public :void {
        my Text $key                     = shift;
        my Iterable::IntWritable $values = shift;
        my Context $context              = shift;

        # ...
        return;
    }
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


my Boolean $b = $job->waitForCompletion(Boolean.true);
if (!$b) {
  die "error with job!";
}


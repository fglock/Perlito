package IOException { import => "java.io.IOException" };
package Configuration { import => "org.apache.hadoop.conf.Configuration" };
package HBaseConfiguration { import => "org.apache.hadoop.hbase.HBaseConfiguration" };
package Get { import => "org.apache.hadoop.hbase.client.Get" };
package HTable { import => "org.apache.hadoop.hbase.client.HTable" };
package Result { import => "org.apache.hadoop.hbase.client.Result" };
package Bytes { import => "org.apache.hadoop.hbase.util.Bytes" };
package Scan { import => 'org.apache.hadoop.hbase.client.Scan' };
package ResultScanner { import => 'org.apache.hadoop.hbase.client.ResultScanner' };

# Instantiating Configuration class
my Configuration $config = HBaseConfiguration->create();

# Instantiating HTable class
my HTable $table = HTable->new( $config, "prices" );

#################### SCAN ##############################
my Scan $scan = Scan->new();

$scan->setStartRow(Bytes->toBytes("100002303"));
$scan->setStopRow(Bytes->toBytes("100005303"));
$scan->addColumn(Bytes->toBytes("d"), Bytes->toBytes("max_price"));

my ResultScanner $resultScanner = $table->getScanner($scan);

# not declared as Result (java object) since we want lValue (perl object)
# containing PlResult (perl object)
my $scan_result = $resultScanner->next();

while (defined $scan_result) {

    # cast perl object to java object
    my Result $java_obj_result = $scan_result;

    # TODO: autoamatic casting (my Result $java_obj = $scan_result;)
    my $scanned_max_price =
        Bytes->toString(
            $java_obj_result->getValue(
                 Bytes->toBytes("d"),
                 Bytes->toBytes("max_price")
             )
        )
    ;

    say "max_price $scanned_max_price";

    $scan_result = $resultScanner->next();
}

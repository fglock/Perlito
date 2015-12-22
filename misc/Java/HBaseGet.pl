package IOException { import => "java.io.IOException" };
package Configuration { import => "org.apache.hadoop.conf.Configuration" };
package HBaseConfiguration { import => "org.apache.hadoop.hbase.HBaseConfiguration" };
package Get { import => "org.apache.hadoop.hbase.client.Get" };
package HTable { import => "org.apache.hadoop.hbase.client.HTable" };
package Result { import => "org.apache.hadoop.hbase.client.Result" };
package Bytes { import => "org.apache.hadoop.hbase.util.Bytes" };

# Instantiating Configuration class
my Configuration $config = HBaseConfiguration->create();

# Instantiating HTable class
my HTable $table = HTable->new( $config, "prices" );

# Instantiating Get class
my Get $g = Get->new( Bytes->toBytes("1234") );

# Reading the data
my Result $result = $table->get($g);

# Reading values from Result class object
my $max_price = Bytes->toString( $result->getValue( Bytes->toBytes("d"), Bytes->toBytes("max_price") ) );

# Printing the values
say "max price: " . $max_price;


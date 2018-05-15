
"Perlito to-Java" Examples

    This document is a collection of examples and imortant
    notes about Perlito to-Java compiler usage.


General Information:

    The main motivation of to-Java compilation is to be
    able to use native java APIs with Perl5 syntax.
    The perl code is compiled to java code. The supported
    Perl syntax is a compatible subset of Perl5.

    That model of operation implies two major tradeoffs:

        CON: Some Perl5 modules will not compile. Specifically,
             this applies to XS modules, modules dependent on
             XS modules and modules which use syntax constructs
             outside of supported subset.

        PRO: All the java classes ever written can be used with
             the supported subset of Perl syntax (which is large
             enough for writting useful code).


How to run:

  One-liners
  
    perl perlito5.pl -Isrc5/lib -I. -It -Cjava -e ' my $v; my $x = 3; $v = sub { $x = $x + 1; $_[0] + $x;  }; say $v->(123), " ", $x;  ' > Main.java ; javac Main.java ; java Main


  Perl scripts

    perl perlito5.pl -Isrc5/lib -I. -It -Cjava myscript.pl  > Main.java ; javac Main.java ; java Main


Example 1: (basic operations: file => ./benchmark.pl):

    This code does not have any dependencies and will
    run in both Perl5 and Java-compiled version:

        my $count;

        my $i = 0;
        while ( $i < 400 ) {
            my $j = 0;
            while ( $j < 400 ) {
                my $k = 0;
                while ( $k < 400 ) {
                    $k = $k + 1;
                    $count = $count + 1;
                }
                $j = $j + 1;
            }
            $i = $i + 1;
        }

        print "done $count\n";

    This is fairly simple benchmark. In our runs java
    was ~3 times faster.


Example 2: (Using regexes: file => benchmark_regex.pl)

    This is a simple regex benchmark:

        my $count;
        my $i = 0;
        while ( $i < 400 ) {
            my $j = 0;
            while ( $j < 400 ) {
                my $k = 0;
                while ( $k < 400 ) {
                    $k = $k + 1;
                    $count = $count + 1 if $k =~ /42/;
                }
                $j = $j + 1;
            }
            $i = $i + 1;
        }
        print "done $count\n";


    In our benchmarks, the java code and the perl code
    performed roughly the same. This is not surprising
    since even though Java is a compiled language, Perl
    is made for text processing and its regex engine is
    writen in highly optimized C code.



Example 3: (HBase Get example: file => HBaseGet.pl)

    This example show how to create java object and call java
    methods.

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
        my HTable $table = new HTable( $config, "prices" );

        # Instantiating Get class
        my Get $g = new Get( Bytes->toBytes("1234") );

        # Reading the data
        my Result $result = $table->get($g);

        # Reading values from Result class object
        my $max_price = Bytes->toString( $result->getValue( Bytes->toBytes("d"), Bytes->toBytes("max_price") ) );

        # Printing the values
        say "max price: " . $max_price;


Example 4: (HBase Scanner example: file => HBaseScanner.pl)

    This example follows up on example 3 with the addition of casting java
    objects back to perl objects.

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
        my HTable $table = new HTable( $config, "prices" );

        my Scan $scan = new Scan();

        $scan->setStartRow(Bytes->toBytes("100002303"));
        $scan->setStopRow(Bytes->toBytes("100005303"));
        $scan->addColumn(Bytes->toBytes("d"), Bytes->toBytes("max_price"));

        my ResultScanner $resultScanner = $table->getScanner($scan);

        # Not declared as Result since we want perl object
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


    Casting from java to perl is currently manual. In the future this
    will be automatic, like:


        my Result $java_obj = $scan_result;




Perlito + Spark
===============

Download and start Spark
--------------

get Spark from https://spark.apache.org/downloads.html

```sh
tar -xzvf spark-2.4.4-bin-hadoop2.7.tgz

export SPARK_HOME=spark-2.4.4-bin-hadoop2.7

$SPARK_HOME/sbin/start-master.sh
```

view the status screen at http://localhost:8080

start slave

```sh
$SPARK_HOME/sbin/start-slave.sh spark://localhost:7077
```

quick test: start shell

```sh
$SPARK_HOME/bin/spark-shell
```

Start Perlito
-------------

check that `spark.pl` is valid Perl:

```
perl -c -I src5/lib/Perlito5X/Java misc/Spark/spark.pl
```

run:

```
export CLASSPATH=.:spark-2.4.4-bin-hadoop2.7/jars/*

java org.perlito.Perlito5.Main -I src5/lib misc/Spark/spark.pl 3
```

Note: example translated from Java to Perl from:
https://github.com/apache/spark/blob/master/examples/src/main/java/org/apache/spark/examples/JavaSparkPi.java


Teardown
--------

```sh
$ $SPARK_HOME/sbin/stop-slave.sh
$ $SPARK_HOME/sbin/stop-master.sh
```

See also
--------

Spark + Nashorn (deprecated?)

https://github.com/EclairJS/eclairjs-nashorn

https://stackoverflow.com/questions/49829959/apache-spark-implementation-in-nodejs-application



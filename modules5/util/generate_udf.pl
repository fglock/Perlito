use strict;
use warnings;

use feature 'say';

use Getopt::Long;

use Perlito5X::Java::Hive::UDF::Simple;

# Run from perlito root folder with:
# perl -I modules5/lib modules5/util/generate_udf.pl --from-file modules5/util/perl_transform.pl

my $transform_script_file_name;

GetOptions (
    "from-file=s"   => \$transform_script_file_name
);

die "Transform script not specifed!"
    unless $transform_script_file_name;

my $udf = Perlito5X::Java::Hive::UDF::Simple->new({

    java_imports => {
        Text => 'org.apache.hadoop.io.Text'
    },

    udf_name => "MyUDF",
    
    udf_signature => {
        input => ["Text"], # input java type
        return => "Text",  # return java type
    },
    
    transform => $transform_script_file_name

});

$udf->generate();


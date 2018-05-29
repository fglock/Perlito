use strict;


package header  { java_path => 'org.perlito.udfs'                   };
package Boolean {                                                   };
package String  {                                                   };
package UDF     { import    => 'org.apache.hadoop.hive.ql.exec.UDF' };

package Text { import => 'org.apache.hadoop.io.Text' };
package MyUDF {

    extends => 'UDF',

    decl    => ['public', 'final'],

    'Java::inline' => "
          public MyUDF() {
            super();
            Main.init();
        }
    ",

    methods => [
        evaluate => {
            decl   => ['public', 'final'],
            args   => ['Text'],
            return => 'Text',
            code   => 'Impl::evaluate'
        },
    ],
};
################################
# Implement overrided methods:
package Impl;
sub transform {
    my $column = shift;

    # Take care of types (unwrap Text object, cast to String, wrap back to perl var)
    my $pString = $column->to_Text()->toString();

    # do the work
    my @checkin = $pString=~m/checkin=(\d{4}-\d{2}-\d{2})/g;

    # return as wraped Text
    my $result = Text->new(join("-", @checkin));
    return $result;
}
sub evaluate {
    my $udfJavaObject = shift;
    my $pColumn       = shift;

    my $result = transform($pColumn);

    return $result;
}


package Perlito5X::Java::Hive::UDF::Simple;

use strict;
use warnings;

use feature 'say';

sub new {
    my ($class, $spec) = @_;
    return bless { spec => $spec };
}

sub generate {
    my $self = shift;
    
    my $code = $self->_generate_code($self->{spec});

    say $code;
}

sub _generate_code {
    my $self = shift;
    my $spec = $self->{spec};

    # header
    my $boiler_plate =<<'HEADER';
package header  { java_path => 'org.perlito.udfs'                   };
package Boolean {                                                   };
package String  {                                                   };
package UDF     { import    => 'org.apache.hadoop.hive.ql.exec.UDF' };
HEADER

    # add deps
    my $deps = $spec->{java_imports};
    map {
            my $dep = "\npackage "
                    . $_
                    . " { import => \'"
                    . $deps->{$_}
                    . "\' };" . "\n";
            $boiler_plate .= $dep;

        } keys %$deps;

    ###########################################
    # Declare extend class & override methods):
    my $extends = <<'EXTENDS';
package __CLASS_NAME__ {

    extends => 'UDF',

    decl    => ['public', 'final'],

    'Java::inline' => "
          public __CLASS_NAME__() {
            super();
            Main.init();
        }
    ",

    methods => [
        evaluate => {
            decl   => ['public', 'final'],
            args   => [__IN_TYPES__],
            return => '__OUT_TYPE__',
            code   => 'Impl::evaluate'
        },
    ],
};
EXTENDS

    $extends=~s/__CLASS_NAME__/$spec->{udf_name}/g;
    $extends=~s/__OUT_TYPE__/$spec->{udf_signature}->{return}/g;
    
    my $input_signature_string = join(",", map { "\'" . $_ . "\'" } @{ $spec->{udf_signature}->{input} });
    $extends=~s/__IN_TYPES__/$input_signature_string/g;
   
    $boiler_plate .= $extends;

    my $impl = <<'IMPL';
################################
# Implement overrided methods:
package Impl;
IMPL

    my $transform;
    my $filename = $spec->{transform};
    open(my $fh, '<', $filename)
        or die "Could not open file '$filename' $!";
    while (my $row = <$fh>) {
      $transform .= $row;
    }
    close $fh;

    $impl .= $transform;

    $impl .=<<'IMPL';
sub evaluate {
    my $udfJavaObject = shift;
    my $pColumn       = shift;

    my $result = transform($pColumn);
    
    return $result;    
}
IMPL
    
    $boiler_plate .= $impl;

    return $boiler_plate;
}

1;

package Java;

sub inline {
    # this is a placeholder,
    # the "real" Java::inline is a macro defined at Perlito5/Java/Apply.pm
}

sub type ($$) {
    # from Nashorn: var HashMap = Java.type("java.util.HashMap")
    my ( $class, $type ) = @_;
    my $array = "";
    while ( substr( $type, -2, 2 ) eq "[]" ) {
        # int[][]  => [[I
        # Object[] => [Ljava.lang.Object;
        $type = substr( $type, 0, -2 );
        $array .= "[";
    }
    if    ( $type eq "boolean" ) { $type = $array . "Z"; }
    elsif ( $type eq "byte" )    { $type = $array . "B"; }
    elsif ( $type eq "char" )    { $type = $array . "C"; }
    elsif ( $type eq "double" )  { $type = $array . "D"; }
    elsif ( $type eq "float" )   { $type = $array . "F"; }
    elsif ( $type eq "int" )     { $type = $array . "I"; }
    elsif ( $type eq "long" )    { $type = $array . "J"; }
    elsif ( $type eq "short" )   { $type = $array . "S"; }
    elsif ( $array )             { $type = $array . "L" . $type . ";" }
    _type($type);
}

sub _type ($) {
    eval {
        Java::inline q{
            Class.forName( List__.aget(0).toString() )
        }
    };
}

1;


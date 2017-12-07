package Java;

sub inline {
    # this is a placeholder,
    # the "real" Java::inline is a macro defined at Perlito5/Java/Apply.pm
}

sub type ($$) {
    # from Nashorn: var HashMap = Java.type("java.util.HashMap")
    eval {
        Java::inline q{
            Class.forName( List__.aget(1).toString() )
        }
    };
}

1;


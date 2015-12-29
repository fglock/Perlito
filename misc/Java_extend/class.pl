# "extends" example

package JavaObject {
    import => 'java.lang.Object',
}

package JavaExtended {
    extends => 'JavaObject',
    methods => [
        instance_meth => {
            decl => [ "public", "Int" ],
            args => [ "Int" ],     # this/$self is added to the Perl method arguments
            code => "MyClass::instance_meth",
        },
        class_meth => {
            decl => [ "public", "static", "Int" ],
            args => [ "Int" ],     # class name is added to the Perl method arguments
            code => "MyClass::class_meth",
        },
    ],
}

package MyClass {
    sub instance_meth {
        my JavaExtended $self = shift;
        my Int $param1 = shift;
        return $param1 + 1;
    }
    sub class_meth {
        my $class = shift;
        my Int $param1 = shift;
        return $param1 + 1;
    }
}

1;

__END__


package JavaObject {
    import => 'java.lang.Object',
}

package MyClass {
    extends => 'JavaObject';
    sub MODIFY_CODE_ATTRIBUTES   { }

    sub instance_meth :public :Int {
        my MyClass $self = shift;
        my Int $param1 = shift;
        return $param1 + 1;
    }
    sub class_meth :public :static :Int {
        my Int $param1 = shift;
        return $param1 + 1;
    }

}

1;

__END__

    # generated Java code:

    class MyClass extends java.lang.Object {
        public Int instance_meth(Int param1) throws Exception {
            PlObject[] res = Main.apply("MyClass::instance_meth", this, param1);
            return res[0].to_Int();
        }
        public Int class_meth(Int param1) throws Exception {
            PlObject[] res = Main.apply("MyClass::class_meth", param1);
            return res[0].to_Int();
        }
    }



    # possible Perl "macros" for shortening

    use Java::class MyClass => {
        extends => 'JavaObject',
    
        public_Int(instance_meth => {
            $this # avaiable
        })
    
        public_static_Int(class_meth => {
            # $this not available
        })
    };



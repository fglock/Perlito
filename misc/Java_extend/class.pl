
package MyClass {
    extends => 'Object';
    sub MODIFY_CODE_ATTRIBUTES   { }

    sub meth :public :Int {
        my Int $param1 = shift;
        return $param1 + 1;
    }

}

1;

__END__

    class MyClass extends Object {
        public Int meth(Int param1) throws Exception {
            PlObject[] res = Main.apply("MyClass::meth", param1);
            return res[0].to_Int();
        }
    }



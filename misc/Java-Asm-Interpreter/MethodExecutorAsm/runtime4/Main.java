public class Main {
    public static void main(String[] args) {
        PerlVariable var = new PerlVariable();

        // Set and get an integer value
        var.setInteger(42);
        System.out.println("Integer: " + var.getInteger());

        // Set and get a string value
        var.setString("Hello, Perl!");
        System.out.println("String: " + var.getString());

        // Set and get a double value
        var.setDouble(3.14159);
        System.out.println("Double: " + var.getDouble());

        // Set and get an array reference
        var.setArrayRef(new Object[]{"a", "b", "c"});
        System.out.println("Array Reference: " + Arrays.toString(var.getArrayRef()));

        // Set and get a hash reference
        Map<String, Object> hash = new HashMap<>();
        hash.put("key1", 1);
        hash.put("key2", "value2");
        var.setHashRef(hash);
        System.out.println("Hash Reference: " + var.getHashRef());

        // Set and get a code reference
        var.setCodeRef(() -> System.out.println("Hello from code reference!"));
        var.getCodeRef().run();

        // Set and get an object
        var.setObject(new Main());
        System.out.println("Object: " + var.getObject());

        // Set to undefined and check
        var.setUndefined();
        System.out.println("Is Undefined: " + var.isUndefined());
    }
}

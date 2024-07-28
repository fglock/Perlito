public class Main {
    public static void main(String[] args) {
        PerlVariable var1 = new PerlVariable(new PerlInt(10));
        PerlVariable var2 = new PerlVariable(new PerlDouble(20.5));
        PerlVariable var3 = new PerlVariable(new PerlString("Hello"));

        PerlVariable result = var1.add(var2); // Should return a PerlVariable with PerlDouble value 30.5
        System.out.println("10 + 20.5 = " + ((PerlDouble) result.getValue()).getValue());

        result = var1.multiply(var2); // Should return a PerlVariable with PerlDouble value 205.0
        System.out.println("10 * 20.5 = " + ((PerlDouble) result.getValue()).getValue());

        result = var3.add(new PerlVariable(new PerlString(" World"))); // Should return a PerlVariable with PerlString value "Hello World"
        System.out.println("\"Hello\" + \" World\" = " + result.getValue().toString());

        // Example of updating the value without changing the reference
        PerlVariable ref1 = var1;
        ref1.setValue(new PerlInt(42));
        System.out.println("Updated value of var1: " + ((PerlInt) var1.getValue()).getValue());
    }
}

/**
 * The CustomClassLoader class extends the ClassLoader and provides a method
 * to define a class from a byte array. This can be useful for dynamically
 * loading classes at runtime from bytecode that is generated or retrieved
 * from a non-standard source.
 *
 * <p>This class overrides the defineClass method to allow for defining a class
 * using a byte array that contains the class data.</p>
 */
public class CustomClassLoader extends ClassLoader {

    /**
     * Defines a class using the given name and byte array containing the class data.
     * This method delegates to the protected defineClass method of the ClassLoader
     * class, specifying the byte array and its length.
     *
     * @param name the expected binary name of the class
     * @param b the byte array containing the class data
     * @return the resulting Class object that was created from the byte array
     * @throws ClassFormatError if the data in the byte array does not represent a valid class
     */
    public Class<?> defineClass(String name, byte[] b) {
        return defineClass(name, b, 0, b.length);
    }
}


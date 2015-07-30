/*
    testing import from Perl
    example:

        package Sample { import => 'misc.Java.Sample' }

    compile:

        javac -d . misc/Java/Sample.java
*/

package misc.Java;

public class Sample {

    public Sample() {
    }
    public int to_int() {
        return 123;
    }
    public static Sample[] lots_of_it() {
        return new Sample[]{ new Sample(), new Sample() };
    }
}


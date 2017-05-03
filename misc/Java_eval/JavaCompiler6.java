//
//  $ . make_jar.sh
//  $ cd misc/Java_eval
//  $ javac -cp ../../perlito5-lib.jar JavaCompiler6.java
//
//  $ java -cp '.:../../perlito5-lib.jar' JavaCompiler6 -e ' my $x = 123; say ($x * 3 ) '
//

import org.perlito.Perlito5.*;

public class JavaCompiler6
{
    public static void main(String[] args) throws Exception
    {
        String source = "for my $x (4,5,6) { say $x + 1 }";
        if (args.length > 0) {
            System.out.println("args " + args[0]);
            if (args[0].equals("-e")) {
                System.out.println("args " + args[1]);
                source = args[1];
            }
        }

        System.out.println("initializing Perlito5.Main");
        try {
            org.perlito.Perlito5.Main.main( new String[]{} );
        }
        catch(Exception e) {
            System.out.println("Errors in main()");
        }

        try {
            PlObject res = PlJavaCompiler.eval_string(source);
            System.out.println(res);
        }
        catch(Exception e) {
            System.out.println("Errors in eval_string()");
        }
    }
}



# java -jar perlito5.jar -I src5/lib misc/Java_asm/perlito_bytecode.pl

use strict;
use Java;


my $CompiledCode = Java->type("org.perlito.Perlito5.CompiledCode");
my $PlJavaCompiler = Java->type("org.perlito.Perlito5.PlJavaCompiler");

my $class_loader = $PlJavaCompiler->classLoader;

say "ClassLoader is: ", $class_loader;

my $class_name = "HelloWorld";
my $compiled_code = $CompiledCode->new($class_name);

say "CompiledCode is: ", $compiled_code;

$class_loader->addCode($compiled_code);

my $stream = $compiled_code->openOutputStream();

my @class_data = ( qw/
     202 254 186 190 0 0 0 53 0 28 1 0 10 72 101 108 108 111 87 111 114 108 100 7 0 1 1 0 16 106 97 118 97 47 108 97 110 103 47 79 98 106 101 99 116 7 0 3 1 0 15 72 101 108 108 111 87 111 114 108 100 46 106 97 118 97 1 0 6 60 105 110 105 116 62 1 0 3 40 41 86 12 0 6 0 7 10 0 4 0 8 1 0 8 109 121 77 101 116 104 111 100 1 0 16 106 97 118 97 47 108 97 110 103 47 83 121 115 116 101 109 7 0 11 1 0 3 111 117 116 1 0 21 76 106 97 118 97 47 105 111 47 80 114 105 110 116 83 116 114 101 97 109 59 12 0 13 0 14 9 0 12 0 15 1 0 12 72 101 108 108 111 44 32 87 111 114 108 100 8 0 17 1 0 19 106 97 118 97 47 105 111 47 80 114 105 110 116 83 116 114 101 97 109 7 0 19 1 0 7 112 114 105 110 116 108 110 1 0 21 40 76 106 97 118 97 47 108 97 110 103 47 83 116 114 105 110 103 59 41 86 12 0 21 0 22 10 0 20 0 23 1 0 4 67 111 100 101 1 0 15 76 105 110 101 78 117 109 98 101 114 84 97 98 108 101 1 0 10 83 111 117 114 99 101 70 105 108 101 0 33 0 2 0 4 0 0 0 0 0 2 0 1 0 6 0 7 0 1 0 25 0 0 0 29 0 1 0 1 0 0 0 5 42 183 0 9 177 0 0 0 1 0 26 0 0 0 6 0 1 0 0 0 1 0 9 0 10 0 7 0 1 0 25 0 0 0 37 0 2 0 0 0 0 0 9 178 0 16 18 18 182 0 24 177 0 0 0 1 0 26 0 0 0 10 0 2 0 0 0 3 0 8 0 4 0 1 0 27 0 0 0 2 0 5
/ );

$stream->write( $_ ) for @class_data;
say "wrote into CompiledCode: $stream";

my $class1 = $class_loader->loadClass($class_name);

say "got class: ", $class1;

$_ = $class1;

Java::inline q{
            ( (Class<?>) ( (PlJavaObject) PlV.Scalar_ARG.get() ).toJava() )
                .getMethod("myMethod", new Class[]{})
                .invoke(null)
};


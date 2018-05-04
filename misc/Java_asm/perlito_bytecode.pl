
# java -jar perlito5.jar -I src5/lib misc/Java_asm/perlito_bytecode.pl

use strict;
use Java;


my $CompiledCode = Java->type("org.perlito.Perlito5.CompiledCode");
my $PlJavaCompiler = Java->type("org.perlito.Perlito5.PlJavaCompiler");

my $class_loader = $PlJavaCompiler->classLoader;

say "ClassLoader is: ", $class_loader;

my $compiled_code = $CompiledCode->new("testClass");

say "CompiledCode is: ", $compiled_code;

$class_loader->addCode($compiled_code);

my $stream = $compiled_code->openOutputStream();

$stream->write( 123 );
say "writing into CompiledCode: $stream";



# here we import the Java packages into Perl packages:
package ScriptEngineManager {
    import => "javax.script.ScriptEngineManager"
};
package ScriptEngine {
    import => "javax.script.ScriptEngine"
};

eval {
    # these are Perl variables, but they are also Java objects
    # so we have to declare which package they belong to:
    my ScriptEngineManager $manager = ScriptEngineManager->new();
    my ScriptEngine $engine = $manager->getEngineByName("JavaScript");
    
    # now we can call the Rhino javascript engine:
    $engine->eval(
        'print( "JS thinks that the result is " + ( 1+1 ) + "\n" );'
    );
    1;
}
  or print "JS returned error: $@\n";


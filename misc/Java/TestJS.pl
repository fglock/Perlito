package ScriptEngineManager {
    import => "javax.script.ScriptEngineManager"
};
package ScriptEngine {
    import => "javax.script.ScriptEngine"
};

my ScriptEngineManager $manager = new ScriptEngineManager();
my ScriptEngine $engine = $manager->getEngineByName("JavaScript");

$engine->eval(
    'print( "JS thinks that the result is " + ( 1+1 ) + "\n" );'
);



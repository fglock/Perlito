use v5;

package Perlito5::Java::JavaxScript;
use strict;

sub meta_file_name {
    "META-INF/services/javax.script.ScriptEngineFactory"
}

sub emit_meta_file {
    "org.perlito.Perlito5.PerlitoEngineFactory\n"
}

sub emit_java {
    return <<'EOT'
class PerlitoEngineFactory implements javax.script.ScriptEngineFactory {
    @Override
    public String getEngineName() {
        return "perlito5";
    }
    @Override
    public String getEngineVersion() {
        return "1.0";
    }
    @Override
    public List<String> getExtensions() {
        return Arrays.asList("pl");
    }
    @Override
    public String getLanguageName() {
        return "Perl";
    }
    @Override
    public String getLanguageVersion() {
        return "5.26.0";
    }
    @Override
    public String getMethodCallSyntax(String obj, String m, String[] args) {
        String ret = obj;
        ret += "->" + m + "(";
        for (int i = 0; i < args.length; i++) {
            ret += args[i];
            if (i == args.length - 1)
                ret += ")";
            else
                ret += ",";
        }
        return ret;
    }
    @Override
    public List<String> getMimeTypes() {
        // https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=229148
        return Arrays.asList("text/x-perl");
    }
    @Override
    public List<String> getNames() {
        return Arrays.asList("perl", "perl5", "perlito", "perlito5");
    }
    @Override
    public String getOutputStatement(String o) {
        return "print " + o;
    }
    @Override
    public Object getParameter(String p) {
        if (p == null)
            return null;
        if (p.equals(ScriptEngine.ENGINE))
            return getEngineName();
        if (p.equals(ScriptEngine.ENGINE_VERSION))
            return getEngineVersion();
        if (p.equals(ScriptEngine.NAME))
            return "perlito5";
        if (p.equals(ScriptEngine.LANGUAGE))
            return getLanguageName();
        if (p.equals(ScriptEngine.LANGUAGE_VERSION))
            return getLanguageVersion();
        return null;
    }
    @Override
    public String getProgram(String[] lines) {
        StringBuffer ret = new StringBuffer();
        for (int i = 0; i < lines.length; i++) {
            ret.append(lines[i]);
            ret.append(";\n");
        }
        return ret.toString();
    }
    @Override
    public ScriptEngine getScriptEngine() {
        try {
            PerlitoScriptEngine e = new PerlitoScriptEngine();
            e.setFactory(this);
            return e;
        } catch (ScriptException e) {
            throw new RuntimeException(e);
        }
    }
}
class PerlitoEngine implements javax.script.ScriptEngine {

}
EOT

} # end of emit_java()

1;

__END__

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/programmer_guide/index.html



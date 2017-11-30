use v5;

package Perlito5::Java::JavaxScript;
use strict;

sub meta_file_name {
    "META-INF/services/javax.script.ScriptEngineFactory"
}

sub emit_meta_file {
    "org.perlito.Perlito5.Perlito5ScriptEngineFactory\n"
}

sub emit_java_EngineFactory {
    # TODO - get constants from Perlito5::Runtime

    return <<'EOT'
package org.perlito.Perlito5;

import javax.script.*;
import java.util.*;
import java.io.*;

public final class Perlito5ScriptEngineFactory implements javax.script.ScriptEngineFactory {

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
        return Arrays.asList("text/x-perl", "text/x-perl5");
    }
    @Override
    public List<String> getNames() {
        return Arrays.asList("perl", "perl5", "perlito", "perlito5", "Perl", "Perl5", "Perlito", "Perlito5");
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
        if (p.equals("THREADING"))
            return null;    // not thread-safe
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
        org.perlito.Perlito5.LibPerl.init();
        org.perlito.Perlito5.Main.main(new String[]{"-Cinit"});
        Perlito5ScriptEngine e = new Perlito5ScriptEngine();
        e.setFactory(this);
        return e;
    }
}
EOT

} # end of emit_java_EngineFactory()

sub emit_java_Engine {
    return <<'EOT'
package org.perlito.Perlito5;

import javax.script.*;
import java.util.*;
import java.io.*;

class Perlito5ScriptEngine implements javax.script.ScriptEngine {

    protected ScriptContext context;
    private Perlito5ScriptEngineFactory factory;

    public Perlito5ScriptEngine() {
        // TODO
        context = new SimpleScriptContext();
    }
    public Perlito5ScriptEngine(Bindings n) {
        this();
        if (n == null) {
            throw new NullPointerException("n is null");
        }
        context.setBindings(n, ScriptContext.ENGINE_SCOPE);
    }

    @Override
    public ScriptEngineFactory getFactory() {
        synchronized (this) {
            if (factory == null) {
                factory = new Perlito5ScriptEngineFactory();
            }
        }
        return factory;
    }
    void setFactory(final Perlito5ScriptEngineFactory owningFactory) {
        factory = owningFactory;
    }

    public void setContext(ScriptContext ctxt) {
        if (ctxt == null) {
            throw new NullPointerException("null context");
        }
        context = ctxt;
    }
    public ScriptContext getContext() {
        return context;
    }
    public Bindings createBindings() {
        return new SimpleBindings();
    }
    public Bindings getBindings(int scope) {

        if (scope == ScriptContext.GLOBAL_SCOPE) {
            return context.getBindings(ScriptContext.GLOBAL_SCOPE);
        } else if (scope == ScriptContext.ENGINE_SCOPE) {
            return context.getBindings(ScriptContext.ENGINE_SCOPE);
        } else {
            throw new IllegalArgumentException("Invalid scope value.");
        }
    }
    public void setBindings(Bindings bindings, int scope) {

        if (scope == ScriptContext.GLOBAL_SCOPE) {
            context.setBindings(bindings, ScriptContext.GLOBAL_SCOPE);;
        } else if (scope == ScriptContext.ENGINE_SCOPE) {
            context.setBindings(bindings, ScriptContext.ENGINE_SCOPE);;
        } else {
            throw new IllegalArgumentException("Invalid scope value.");
        }
    }
    public void put(String key, Object value) {

        Bindings nn = getBindings(ScriptContext.ENGINE_SCOPE);
        if (nn != null) {
            nn.put(key, value);
        }

    }
    public Object get(String key) {
        Bindings nn = getBindings(ScriptContext.ENGINE_SCOPE);
        if (nn != null) {
            return nn.get(key);
        }
        return null;
    }
    public Object eval(Reader reader, Bindings bindings ) throws ScriptException {
        ScriptContext ctxt = getScriptContext(bindings);
        return eval(reader, ctxt);
    }
    public Object eval(String script, Bindings bindings) throws ScriptException {
        ScriptContext ctxt = getScriptContext(bindings);
        return eval(script , ctxt);
    }
    public Object eval(Reader reader) throws ScriptException {
        return eval(reader, context);
    }
    public Object eval(String script) throws ScriptException {
        return eval(script, context);
    }
    public Object eval(Reader reader, ScriptContext ctxt) throws ScriptException {
        StringBuilder buf = new StringBuilder();
        boolean eof = false;
        while (!eof) {
            int len = 1000;
            char[] c = new char[len];
            int num_chars = 0;
            try {
                num_chars = reader.read(c, 0, len);
                if (num_chars > 0) {
                    String s = new String(c, 0, num_chars);
                    buf.append(s);
                }
            }
            catch(IOException e) {
                throw new ScriptException(e);
            }
            if (num_chars <= 0) {
                eof = true;
            }
        }
        String s;
        s = buf.toString();
        return eval(s, ctxt);
    }
    public Object eval(String script, ScriptContext ctxt) throws ScriptException {
        Object[] ret = org.perlito.Perlito5.Main.apply( "Perlito5::eval_string", script );
        PlObject perlErr = PlV.sget("main::@");
        if (perlErr.to_boolean()) {
            throw new ScriptException(perlErr.toString());
        }
        if (ret.length > 0) {
            return ret[0];
        }
        return null;
    }

    protected ScriptContext getScriptContext(Bindings nn) {

        SimpleScriptContext ctxt = new SimpleScriptContext();
        Bindings gs = getBindings(ScriptContext.GLOBAL_SCOPE);

        if (gs != null) {
            ctxt.setBindings(gs, ScriptContext.GLOBAL_SCOPE);
        }

        if (nn != null) {
            ctxt.setBindings(nn,
                    ScriptContext.ENGINE_SCOPE);
        } else {
            throw new NullPointerException("Engine scope Bindings may not be null.");
        }

        ctxt.setReader(context.getReader());
        ctxt.setWriter(context.getWriter());
        ctxt.setErrorWriter(context.getErrorWriter());

        return ctxt;

    }
}
EOT

} # end of emit_java_Engine()

1;

__END__

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/programmer_guide/index.html



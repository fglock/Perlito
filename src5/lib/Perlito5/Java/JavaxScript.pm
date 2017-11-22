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

}
class PerlitoEngine implements javax.script.ScriptEngine {

}
EOT

} # end of emit_java()

1;

__END__

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/

https://docs.oracle.com/javase/6/docs/technotes/guides/scripting/programmer_guide/index.html



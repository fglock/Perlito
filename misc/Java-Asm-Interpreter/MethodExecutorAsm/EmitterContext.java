import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

public class EmitterContext {
    public final ScopedSymbolTable symbolTable;
    public final Label returnLabel;
    public final MethodVisitor mv;

    public EmitterContext(ScopedSymbolTable symbolTable, MethodVisitor mv) {
        this.symbolTable = symbolTable;
        this.returnLabel = new Label(); // Initialize the global return label
        this.mv = mv;
    }
}


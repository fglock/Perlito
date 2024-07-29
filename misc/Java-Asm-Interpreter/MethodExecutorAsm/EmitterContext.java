import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

public class EmitterContext {
    public final ScopedSymbolTable symbolTable;
    public final Label returnLabel;
    public final MethodVisitor mv;
    public final ContextType contextType; // Use the enum here
    public final boolean isBoxed; // true for boxed object, false for native object

    public EmitterContext(ScopedSymbolTable symbolTable, MethodVisitor mv, ContextType contextType, boolean isBoxed) {
        this.symbolTable = symbolTable;
        this.returnLabel = new Label(); // Initialize the global return label
        this.mv = mv;
        this.contextType = contextType;
        this.isBoxed = isBoxed;
    }

    // Method to create a new context with updated contextType and isBoxed
    public EmitterContext with(ContextType contextType, boolean isBoxed) {
        return new EmitterContext(this.symbolTable, this.mv, contextType, isBoxed);
    }
}


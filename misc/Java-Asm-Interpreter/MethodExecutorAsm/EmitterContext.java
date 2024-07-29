import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;

public class EmitterContext {
  public String fileName;
  public String javaClassName;
  public ScopedSymbolTable symbolTable;
  public Label returnLabel;
  public MethodVisitor mv;
  public ContextType contextType; // Use the enum here
  public boolean isBoxed; // true for boxed object, false for native object

  public EmitterContext(
      String fileName,
      String javaClassName,
      ScopedSymbolTable symbolTable,
      Label returnLabel,
      MethodVisitor mv,
      ContextType contextType,
      boolean isBoxed) {
    this.fileName = fileName;
    this.javaClassName = javaClassName;
    this.symbolTable = symbolTable;
    this.returnLabel = returnLabel;
    this.mv = mv;
    this.contextType = contextType;
    this.isBoxed = isBoxed;
  }

  // Method to create a new context with updated contextType and isBoxed
  public EmitterContext with(ContextType contextType, boolean isBoxed) {
    return new EmitterContext(this.fileName, this.javaClassName, this.symbolTable, this.returnLabel, this.mv, contextType, isBoxed);
  }
  public EmitterContext with(ContextType contextType) {
    return new EmitterContext(this.fileName, this.javaClassName, this.symbolTable, this.returnLabel, this.mv, contextType, this.isBoxed);
  }
}

import java.util.Stack;

class ScopedSymbolTable {
    private Stack<SymbolTable> stack = new Stack<>();

    public String fileName;

    public ScopedSymbolTable(String fileName) {
        this.fileName = fileName;
    }

    public void enterScope() {
        stack.push(new SymbolTable());
    }

    public void exitScope() {
        stack.pop();
    }

    public int addVariable(String name) {
        return stack.peek().addVariable(name);
    }

    public int getVariableIndex(String name) {
        for (int i = stack.size() - 1; i >= 0; i--) {
            int index = stack.get(i).getVariableIndex(name);
            if (index != -1) {
                return index;
            }
        }
        return -1;
    }
}


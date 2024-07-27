import java.util.Stack;

class ScopedSymbolTable {
    private Stack<SymbolTable> stack = new Stack<>();

    public String fileName;

    public ScopedSymbolTable(String fileName) {
        this.fileName = fileName;
    }

    public void enterScope() {
        int lastIndex = 0;
        if (!stack.isEmpty()) {
            lastIndex = stack.peek().index;
        }
        stack.push(new SymbolTable());
        stack.peek().index = lastIndex;
    }

    public void exitScope() {
        stack.pop();
    }

    public int addVariable(String name) {
        return stack.peek().addVariable(name);
    }

    public int getVariableIndex(String name) {
        System.out.println(" scope.getVariableIndex " + name);
        for (int i = stack.size() - 1; i >= 0; i--) {
            System.out.println(" scope.lookup level " + i);
            int index = stack.get(i).getVariableIndex(name);
            if (index != -1) {
                return index;
            }
        }
        return -1;
    }

    public int getVariableIndexInCurrentScope(String name) {
        return stack.peek().getVariableIndex(name);
    }
}


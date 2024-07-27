import java.util.Stack;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.util.TreeMap;


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
        // Iterate from innermost scope to outermost
        for (int i = stack.size() - 1; i >= 0; i--) {
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

    public Map<Integer, String> getAllVisibleVariables() {
        Map<Integer, String> visibleVariables = new TreeMap<>();
        Set<String> seenVariables = new HashSet<>();
        // Iterate from innermost scope to outermost
        for (int i = stack.size() - 1; i >= 0; i--) {
            Map<String, Integer> scope = stack.get(i).table;
            for (Map.Entry<String, Integer> entry : scope.entrySet()) {
                if (!seenVariables.contains(entry.getKey())) {
                    visibleVariables.put(entry.getValue(), entry.getKey());
                    seenVariables.add(entry.getKey());
                }
            }
        }
        return visibleVariables;
    }
}


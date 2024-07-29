import java.util.Stack;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.util.TreeMap;

/**
 * A scoped symbol table that supports nested scopes for variable declarations.
 */
class ScopedSymbolTable {
    // A stack to manage nested scopes of symbol tables
    private Stack<SymbolTable> stack = new Stack<>();
    
    // The name of the file associated with this symbol table
    public String fileName;

    /**
     * Constructs a ScopedSymbolTable for a given file.
     * 
     * @param fileName The name of the file.
     */
    public ScopedSymbolTable(String fileName) {
        this.fileName = fileName;
    }

    /**
     * Enters a new scope by pushing a new SymbolTable onto the stack.
     */
    public void enterScope() {
        int lastIndex = 0;
        // If there are existing scopes, get the last index from the current scope
        if (!stack.isEmpty()) {
            lastIndex = stack.peek().index;
        }
        // Push a new SymbolTable onto the stack and set its index
        stack.push(new SymbolTable());
        stack.peek().index = lastIndex;
    }

    /**
     * Exits the current scope by popping the top SymbolTable from the stack.
     */
    public void exitScope() {
        stack.pop();
    }

    /**
     * Adds a variable to the current scope.
     * 
     * @param name The name of the variable to add.
     * @return The index of the variable in the current scope.
     */
    public int addVariable(String name) {
        return stack.peek().addVariable(name);
    }

    /**
     * Retrieves the index of a variable, searching from the innermost to the outermost scope.
     * 
     * @param name The name of the variable to look up.
     * @return The index of the variable, or -1 if the variable is not found.
     */
    public int getVariableIndex(String name) {
        // Iterate from innermost scope to outermost
        for (int i = stack.size() - 1; i >= 0; i--) {
            int index = stack.get(i).getVariableIndex(name);
            if (index != -1) {
                return index;
            }
        }
        return -1;
    }

    /**
     * Retrieves the index of a variable in the current scope.
     * This method is used to track variable redeclarations.
     * 
     * @param name The name of the variable to look up.
     * @return The index of the variable, or -1 if the variable is not found in the current scope.
     */
    public int getVariableIndexInCurrentScope(String name) {
        return stack.peek().getVariableIndex(name);
    }

    /**
     * Retrieves all visible variables from the current scope to the outermost scope.
     * This method is used to track closure variables.
     * The returned TreeMap is sorted by variable index.
     * 
     * @return A TreeMap of variable index to variable name for all visible variables.
     */
    public Map<Integer, String> getAllVisibleVariables() {
        // TreeMap to store variable indices as keys and variable names as values.
        // TreeMap is used to keep the entries sorted by the keys (variable indices).
        Map<Integer, String> visibleVariables = new TreeMap<>();
        
        // HashSet to keep track of variable names that have already been added to visibleVariables.
        // This helps to avoid adding the same variable multiple times if it appears in multiple scopes.
        Set<String> seenVariables = new HashSet<>();
        
        // Iterate from innermost scope (top of the stack) to outermost scope (bottom of the stack).
        for (int i = stack.size() - 1; i >= 0; i--) {
            // Retrieve the symbol table for the current scope.
            Map<String, Integer> scope = stack.get(i).table;
            
            // Iterate through all variables in the current scope.
            for (Map.Entry<String, Integer> entry : scope.entrySet()) {
                // Check if the variable name has already been seen.
                if (!seenVariables.contains(entry.getKey())) {
                    // If not seen, add the variable's index and name to visibleVariables.
                    visibleVariables.put(entry.getValue(), entry.getKey());
                    
                    // Mark the variable name as seen by adding it to seenVariables.
                    seenVariables.add(entry.getKey());
                }
            }
        }
        
        // Return the TreeMap containing all visible variables sorted by their indices.
        return visibleVariables;
    }
}


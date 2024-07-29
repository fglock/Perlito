import java.util.Map;
import java.util.HashMap;

/**
 * A simple symbol table that maps variable names to unique integer indices.
 */
class SymbolTable {
    // A map to store variable names and their corresponding indices
    public Map<String, Integer> table = new HashMap<>();
    
    // A counter to generate unique indices for variables
    public int index = 0;

    /**
     * Adds a variable to the symbol table if it does not already exist.
     * 
     * @param name The name of the variable to add.
     * @return The index of the variable in the symbol table.
     */
    public int addVariable(String name) {
        // Check if the variable is not already in the table
        if (!table.containsKey(name)) {
            // Add the variable with a unique index
            table.put(name, index++);
        }
        // Return the index of the variable
        return table.get(name);
    }

    /**
     * Retrieves the index of a variable from the symbol table.
     * 
     * @param name The name of the variable to look up.
     * @return The index of the variable, or -1 if the variable is not found.
     */
    public int getVariableIndex(String name) {
        // Return the index of the variable, or -1 if not found
        return table.getOrDefault(name, -1);
    }
}


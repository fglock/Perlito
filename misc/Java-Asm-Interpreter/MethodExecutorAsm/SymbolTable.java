import java.util.Map;
import java.util.HashMap;

class SymbolTable {
    private Map<String, Integer> table = new HashMap<>();
    private int nextIndex = 0;

    public int addVariable(String name) {
        if (!table.containsKey(name)) {
            table.put(name, nextIndex++);
        }
        return table.get(name);
    }

    public int getVariableIndex(String name) {
        return table.getOrDefault(name, -1);
    }
}


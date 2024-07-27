import java.util.Map;
import java.util.HashMap;

class SymbolTable {
    private Map<String, Integer> table = new HashMap<>();
    public int index = 1;   // start after subroutine arguments

    public int addVariable(String name) {
        if (!table.containsKey(name)) {
            table.put(name, index++);
        }
        return table.get(name);
    }

    public int getVariableIndex(String name) {
        return table.getOrDefault(name, -1);
    }
}


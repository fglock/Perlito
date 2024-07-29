public class StringNode extends Node {
    String value;

    StringNode(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "String(" + value + ")";
    }
}


public class NumberNode extends Node {
    String value;

    NumberNode(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "Number(" + value + ")";
    }
}


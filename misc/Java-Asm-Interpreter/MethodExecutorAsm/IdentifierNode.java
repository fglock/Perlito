public class IdentifierNode extends Node {
    String name;

    IdentifierNode(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "Identifier(" + name + ")";
    }
}


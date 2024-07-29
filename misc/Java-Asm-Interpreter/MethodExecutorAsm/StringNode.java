public class StringNode implements Node {
    String value;

    StringNode(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return "String(" + value + ")";
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


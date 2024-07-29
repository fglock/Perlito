public class NumberNode implements Node {
    public final String value;

    public NumberNode(String value) {
        this.value = value;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


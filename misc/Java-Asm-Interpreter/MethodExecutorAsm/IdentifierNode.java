public class IdentifierNode implements Node {
    public final String name;

    public IdentifierNode(String name) {
        this.name = name;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


public class BinaryOperatorNode implements Node {
    public final String operator;
    public final Node left;
    public final Node right;

    public BinaryOperatorNode(String operator, Node left, Node right) {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


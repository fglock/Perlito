public class UnaryOperatorNode implements Node {
    public final String operator;
    public final Node operand;

    public UnaryOperatorNode(String operator, Node operand) {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


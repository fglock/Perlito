public class PostfixOperatorNode implements Node {
    String operator;
    Node operand;

    PostfixOperatorNode(String operator, Node operand) {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


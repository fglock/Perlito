public class PostfixOperatorNode extends Node {
    String operator;
    Node operand;

    PostfixOperatorNode(String operator, Node operand) {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public String toString() {
        return "PostfixOperator(" + operator + ", " + operand + ")";
    }
}


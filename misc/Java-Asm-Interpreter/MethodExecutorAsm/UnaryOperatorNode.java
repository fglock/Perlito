public class UnaryOperatorNode extends Node {
    String operator;
    Node operand;

    UnaryOperatorNode(String operator, Node operand) {
        this.operator = operator;
        this.operand = operand;
    }

    @Override
    public String toString() {
        return "UnaryOperator(" + operator + ", " + operand + ")";
    }
}


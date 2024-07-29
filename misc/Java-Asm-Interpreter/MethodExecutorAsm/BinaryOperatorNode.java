public class BinaryOperatorNode extends Node {
    String operator;
    Node left;
    Node right;

    BinaryOperatorNode(String operator, Node left, Node right) {
        this.operator = operator;
        this.left = left;
        this.right = right;
    }

    @Override
    public String toString() {
        return "BinaryOperator(" + operator + ", " + left + ", " + right + ")";
    }
}


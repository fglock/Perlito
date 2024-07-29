public class TernaryOperatorNode extends Node {
    String operator;
    Node condition;
    Node trueExpr;
    Node falseExpr;

    TernaryOperatorNode(String operator, Node condition, Node trueExpr, Node falseExpr) {
        this.operator = operator;
        this.condition = condition;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
    }

    @Override
    public String toString() {
        return "TernaryOperator(" + condition + " ? " + trueExpr + " : " + falseExpr + ")";
    }
}


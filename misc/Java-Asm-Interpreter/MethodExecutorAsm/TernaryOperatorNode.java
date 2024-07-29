public class TernaryOperatorNode implements Node {
    public final String operator;
    public final Node condition;
    public final Node trueExpr;
    public final Node falseExpr;

    public TernaryOperatorNode(String operator, Node condition, Node trueExpr, Node falseExpr) {
        this.operator = operator;
        this.condition = condition;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


/**
 * The TernaryOperatorNode class represents a node in the abstract syntax tree (AST) that holds
 * a ternary operator and its three operands: a condition, a true expression, and a false expression.
 * This class implements the Node interface, allowing it to be visited by a Visitor.
 *
 * <p>The TernaryOperatorNode class is used to encapsulate ternary operations in the AST, providing
 * a way to store and manipulate ternary operators and their operands within the tree structure.</p>
 */
public class TernaryOperatorNode extends AbstractNode {
    /**
     * The ternary operator represented by this node.
     */
    public final String operator;

    /**
     * The condition operand of the ternary operator.
     */
    public final Node condition;

    /**
     * The true expression operand of the ternary operator.
     */
    public final Node trueExpr;

    /**
     * The false expression operand of the ternary operator.
     */
    public final Node falseExpr;

    /**
     * Constructs a new TernaryOperatorNode with the specified operator and operands.
     *
     * @param operator the ternary operator to be stored in this node
     * @param condition the condition operand of the ternary operator
     * @param trueExpr the true expression operand of the ternary operator
     * @param falseExpr the false expression operand of the ternary operator
     */
    public TernaryOperatorNode(String operator, Node condition, Node trueExpr, Node falseExpr, int tokenIndex) {
        this.operator = operator;
        this.condition = condition;
        this.trueExpr = trueExpr;
        this.falseExpr = falseExpr;
        this.tokenIndex = tokenIndex;
    }

    /**
     * Accepts a visitor that performs some operation on this node.
     * This method is part of the Visitor design pattern, which allows
     * for defining new operations on the AST nodes without changing
     * the node classes.
     *
     * @param visitor the visitor that will perform the operation on this node
     */
    @Override
    public void accept(Visitor visitor) throws Exception {
        visitor.visit(this);
    }
}


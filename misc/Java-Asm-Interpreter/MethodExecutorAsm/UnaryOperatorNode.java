/**
 * The UnaryOperatorNode class represents a node in the abstract syntax tree (AST) that holds
 * a unary operator and its operand. This class implements the Node interface, allowing it to be
 * visited by a Visitor.
 *
 * <p>The UnaryOperatorNode class is used to encapsulate unary operations in the AST, providing
 * a way to store and manipulate unary operators and their operands within the tree structure.</p>
 */
public class UnaryOperatorNode extends AbstractNode {
    /**
     * The unary operator represented by this node.
     */
    public final String operator;

    /**
     * The operand on which the unary operator is applied.
     */
    public final Node operand;

    /**
     * Constructs a new UnaryOperatorNode with the specified operator and operand.
     *
     * @param operator the unary operator to be stored in this node
     * @param operand the operand on which the unary operator is applied
     */
    public UnaryOperatorNode(String operator, Node operand, int tokenIndex) {
        this.operator = operator;
        this.operand = operand;
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


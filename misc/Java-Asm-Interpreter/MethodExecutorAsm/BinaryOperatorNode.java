/**
 * The BinaryOperatorNode class represents a node in the abstract syntax tree (AST) that holds
 * a binary operator and its two operands. This class implements the Node interface, allowing it to be
 * visited by a Visitor.
 *
 * <p>The BinaryOperatorNode class is used to encapsulate binary operations in the AST, providing
 * a way to store and manipulate binary operators and their operands within the tree structure.</p>
 */
public class BinaryOperatorNode extends AbstractNode {
    /**
     * The binary operator represented by this node.
     */
    public final String operator;

    /**
     * The left operand of the binary operator.
     */
    public final Node left;

    /**
     * The right operand of the binary operator.
     */
    public final Node right;

    /**
     * Constructs a new BinaryOperatorNode with the specified operator and operands.
     *
     * @param operator the binary operator to be stored in this node
     * @param left the left operand of the binary operator
     * @param right the right operand of the binary operator
     */
    public BinaryOperatorNode(String operator, Node left, Node right, int tokenIndex) {
        this.operator = operator;
        this.left = left;
        this.right = right;
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


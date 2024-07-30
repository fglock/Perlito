/**
 * The PostfixOperatorNode class represents a node in the abstract syntax tree (AST) that holds
 * a postfix operator and its operand. This class implements the Node interface, allowing it to be
 * visited by a Visitor.
 *
 * <p>The PostfixOperatorNode class is used to encapsulate postfix operations in the AST, providing
 * a way to store and manipulate postfix operators and their operands within the tree structure.</p>
 */
public class PostfixOperatorNode implements Node {
    /**
     * The postfix operator represented by this node.
     */
    String operator;

    /**
     * The operand on which the postfix operator is applied.
     */
    Node operand;

    /**
     * Constructs a new PostfixOperatorNode with the specified operator and operand.
     *
     * @param operator the postfix operator to be stored in this node
     * @param operand the operand on which the postfix operator is applied
     */
    PostfixOperatorNode(String operator, Node operand) {
        this.operator = operator;
        this.operand = operand;
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
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


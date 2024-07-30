/**
 * The NumberNode class represents a node in the abstract syntax tree (AST) that holds
 * a numeric value. This class implements the Node interface, allowing it to be visited
 * by a Visitor.
 *
 * <p>The NumberNode class is used to encapsulate numeric literals in the AST, providing
 * a way to store and manipulate numeric values within the tree structure.</p>
 */
public class NumberNode implements Node {
    /**
     * The numeric value represented by this node. It is stored as a string to
     * preserve the exact representation of the number as it appears in the source code.
     */
    public final String value;

    /**
     * Constructs a new NumberNode with the specified numeric value.
     *
     * @param value the numeric value to be stored in this node
     */
    public NumberNode(String value) {
        this.value = value;
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


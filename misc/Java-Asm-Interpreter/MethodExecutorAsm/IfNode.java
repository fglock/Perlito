/**
 * The IfNode class represents a node in the abstract syntax tree (AST) that holds an "if" statement.
 * The parts of the statement are: "condition", "thenBranch" and "elseBranch".
 * This class implements the Node interface, allowing it to be visited by a Visitor.
 *
 * <p>The IfNode class is used to encapsulate "if" in the AST, providing
 * a way to store and manipulate "if" and their parts within the tree structure.</p>
 */
public class IfNode implements Node {
    /**
     * The condition operand of the if statement.
     */
    public final Node condition;

    /**
     * The true block of the if statement.
     */
    public final Node thenBranch;

    /**
     * The false block of the if statement.
     * The elseBranch can be null, another If node, or a Block node
     */
    public final Node elseBranch;

    /**
     * Constructs a new IfNode with the specified operator and operands.
     *
     * @param condition the condition operand of the if statement
     * @param thenBranch the true block of the if statement
     * @param elseBranch the false block of the if statement
     */
    public IfNode(Node condition, Node thenBranch, Node elseBranch) {
        this.condition = condition;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
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


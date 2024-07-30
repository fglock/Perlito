/**
 * The Visitor interface defines a set of methods for visiting different types of nodes
 * in an abstract syntax tree (AST). This interface is part of the Visitor design pattern,
 * which is used to separate algorithms from the objects on which they operate.
 *
 * <p>The Visitor pattern allows you to add new operations to existing object structures
 * without modifying those structures. This is particularly useful in scenarios where
 * you need to perform various operations on a complex object structure, such as an AST,
 * and you want to keep the operations separate from the object structure itself.</p>
 *
 * <p>Each method in this interface corresponds to a specific type of node in the AST.
 * Implementations of this interface will provide the logic for processing each type of node.</p>
 *
 * <p>Why does this interface exist?</p>
 * <ul>
 *   <li>To define a common contract for visiting different types of nodes in an AST.</li>
 *   <li>To enable the implementation of the Visitor design pattern, which promotes
 *       separation of concerns and makes it easier to add new operations to the AST
 *       without modifying the node classes.</li>
 * </ul>
 *
 * <p>What is an interface?</p>
 * <p>An interface in Java is a reference type, similar to a class, that can contain only
 * abstract methods, default methods, static methods, and constants. Interfaces cannot
 * contain instance fields or constructors. An interface defines a contract that classes
 * can implement. By implementing an interface, a class agrees to provide implementations
 * for all of the methods declared in the interface.</p>
 *
 * <p>Interfaces are used to achieve abstraction and multiple inheritance in Java. They
 * allow you to define a set of methods that can be implemented by any class, regardless
 * of where the class is in the inheritance hierarchy.</p>
 */
public interface Visitor {
    /**
     * Visit a BinaryOperatorNode.
     *
     * @param node the BinaryOperatorNode to visit
     */
    void visit(BinaryOperatorNode node);

    /**
     * Visit an IdentifierNode.
     *
     * @param node the IdentifierNode to visit
     */
    void visit(IdentifierNode node);

    /**
     * Visit a BlockNode.
     *
     * @param node the BlockNode to visit
     */
    void visit(BlockNode node);

    /**
     * Visit a ListNode.
     *
     * @param node the ListNode to visit
     */
    void visit(ListNode node);

    /**
     * Visit a NumberNode.
     *
     * @param node the NumberNode to visit
     */
    void visit(NumberNode node);

    /**
     * Visit a PostfixOperatorNode.
     *
     * @param node the PostfixOperatorNode to visit
     */
    void visit(PostfixOperatorNode node);

    /**
     * Visit a StringNode.
     *
     * @param node the StringNode to visit
     */
    void visit(StringNode node);

    /**
     * Visit a IfNode.
     *
     * @param node the IfNode to visit
     */
    void visit(IfNode node);

    /**
     * Visit a TernaryOperatorNode.
     *
     * @param node the TernaryOperatorNode to visit
     */
    void visit(TernaryOperatorNode node);

    /**
     * Visit a UnaryOperatorNode.
     *
     * @param node the UnaryOperatorNode to visit
     */
    void visit(UnaryOperatorNode node);

    // Add other node types as needed
}


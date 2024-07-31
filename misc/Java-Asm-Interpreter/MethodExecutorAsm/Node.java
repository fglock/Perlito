/**
 * The Node interface represents an abstract syntax tree (AST) node.
 * An AST is a tree representation of the abstract syntactic structure
 * of source code written in a programming language. Each node in the
 * tree denotes a construct occurring in the source code.
 *
 * <p>This interface defines a method for accepting a visitor, which
 * allows for implementing the Visitor design pattern. This pattern
 * enables operations to be performed on nodes of the AST without
 * modifying the node classes themselves.</p>
 */
public interface Node {
    /**
     * Accepts a visitor that performs some operation on the node.
     * This method is part of the Visitor design pattern, which allows
     * for defining new operations on the AST nodes without changing
     * the node classes.
     *
     * @param visitor the visitor that will perform the operation on the node
     */
    void accept(Visitor visitor) throws Exception;

    /**
     * Gets the index of the node in the token list.
     *
     * @return the index of the node
     */
    int getIndex();

    /**
     * Sets the index of the node in the token list.
     *
     * @param index the index to set
     */
    void setIndex(int index);
}


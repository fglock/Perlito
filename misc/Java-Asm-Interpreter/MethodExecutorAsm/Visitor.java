public interface Visitor {
    void visit(BinaryOperatorNode node);
    void visit(IdentifierNode node);
    void visit(ListNode node);
    void visit(NumberNode node);
    void visit(PostfixOperatorNode node);
    void visit(StringNode node);
    void visit(TernaryOperatorNode node);
    void visit(UnaryOperatorNode node);
    // Add other node types as needed
}

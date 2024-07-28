interface NodeVisitor {
    void visit(BlockNode node);
    void visit(VariableNode node);
    void visit(NumberNode node);
    void visit(StringNode node);
    void visit(BinaryOpNode node);
    // Add other visit methods for different node types
}

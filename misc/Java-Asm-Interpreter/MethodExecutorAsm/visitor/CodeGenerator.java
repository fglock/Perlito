class CodeGenerator implements NodeVisitor {
    private StringBuilder sb;

    public CodeGenerator() {
        sb = new StringBuilder();
    }

    public String getCode() {
        return sb.toString();
    }

    @Override
    public void visit(BlockNode node) {
        for (Node statement : node.statements) {
            statement.accept(this);
            sb.append(";\n");
        }
    }

    @Override
    public void visit(VariableNode node) {
        sb.append(node.name);
    }

    @Override
    public void visit(NumberNode node) {
        sb.append(node.value);
    }

    @Override
    public void visit(StringNode node) {
        sb.append("\"").append(node.value).append("\"");
    }

    @Override
    public void visit(BinaryOpNode node) {
        node.left.accept(this);
        sb.append(" ").append(node.operator).append(" ");
        node.right.accept(this);
    }

    // Implement other visit methods as needed
}

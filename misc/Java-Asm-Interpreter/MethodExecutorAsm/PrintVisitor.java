/*
 *
 * Usage:
 *
 *   PrintVisitor printVisitor = new PrintVisitor();
 *   node.accept(printVisitor);
 *   return printVisitor.getResult();
 */
public class PrintVisitor implements Visitor {
    private final StringBuilder sb = new StringBuilder();
    private int indentLevel = 0;

    private void appendIndent() {
        for (int i = 0; i < indentLevel; i++) {
            sb.append("  ");
        }
    }

    public String getResult() {
        return sb.toString();
    }

    @Override
    public void visit(NumberNode node) throws Exception {
        appendIndent();
        sb.append("NumberNode: ").append(node.value).append("\n");
    }

    @Override
    public void visit(IdentifierNode node) throws Exception {
        appendIndent();
        sb.append("IdentifierNode: ").append(node.name).append("\n");
    }

    @Override
    public void visit(BinaryOperatorNode node) throws Exception {
        appendIndent();
        sb.append("BinaryOperatorNode: ").append(node.operator).append("\n");
        indentLevel++;
        node.left.accept(this);
        node.right.accept(this);
        indentLevel--;
    }

    @Override
    public void visit(UnaryOperatorNode node) throws Exception {
        appendIndent();
        sb.append("UnaryOperatorNode: ").append(node.operator).append("\n");
        indentLevel++;
        node.operand.accept(this);
        indentLevel--;
    }

    @Override
    public void visit(IfNode node) throws Exception {
        appendIndent();
        sb.append("IfNode:\n");
        indentLevel++;
        node.condition.accept(this);
        node.thenBranch.accept(this);
        if (node.elseBranch != null) {
          node.elseBranch.accept(this);
        }
        indentLevel--;
    }

    @Override
    public void visit(AnonSubNode node) throws Exception {
        appendIndent();
        sb.append("AnonSubNode:\n");
        indentLevel++;
        node.block.accept(this);
        indentLevel--;
    }

    @Override
    public void visit(TernaryOperatorNode node) throws Exception {
        appendIndent();
        sb.append("TernaryOperatorNode: ").append(node.operator).append("\n");
        indentLevel++;
        node.condition.accept(this);
        node.trueExpr.accept(this);
        node.falseExpr.accept(this);
        indentLevel--;
    }

    @Override
    public void visit(PostfixOperatorNode node) throws Exception {
        appendIndent();
        sb.append("PostfixOperatorNode: ").append(node.operator).append("\n");
        indentLevel++;
        node.operand.accept(this);
        indentLevel--;
    }

    @Override
    public void visit(StringNode node) throws Exception {
        appendIndent();
        sb.append("StringNode: ").append(node.value).append("\n");
    }

    @Override
    public void visit(BlockNode node) throws Exception {
        appendIndent();
        sb.append("BlockNode:\n");
        indentLevel++;
        for (Node element : node.elements) {
            element.accept(this);
        }
        indentLevel--;
    }

    @Override
    public void visit(ListNode node) throws Exception {
        appendIndent();
        sb.append("ListNode:\n");
        indentLevel++;
        for (Node element : node.elements) {
            element.accept(this);
        }
        indentLevel--;
    }

    // Add other visit methods as needed
}


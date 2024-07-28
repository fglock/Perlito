import java.util.List;

abstract class Node {
    public abstract void accept(NodeVisitor visitor);
}

class BlockNode extends Node {
    List<Node> statements;

    BlockNode(List<Node> statements) {
        this.statements = statements;
    }

    @Override
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
    }
}

class VariableNode extends Node {
    String name;

    VariableNode(String name) {
        this.name = name;
    }

    @Override
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
    }
}

class NumberNode extends Node {
    String value;

    NumberNode(String value) {
        this.value = value;
    }

    @Override
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
    }
}

class StringNode extends Node {
    String value;

    StringNode(String value) {
        this.value = value;
    }

    @Override
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
    }
}

class BinaryOpNode extends Node {
    Node left;
    String operator;
    Node right;

    BinaryOpNode(Node left, String operator, Node right) {
        this.left = left;
        this.operator = operator;
        this.right = right;
    }

    @Override
    public void accept(NodeVisitor visitor) {
        visitor.visit(this);
    }
}

// Abstract syntax tree (AST) node classes
public interface Node {
    void accept(Visitor visitor);
}


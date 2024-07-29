import java.util.*;

public class ListNode implements Node {
    List<Node> elements;

    ListNode(List<Node> elements) {
        this.elements = elements;
    }

    @Override
    public String toString() {
        return "List(" + elements + ")";
    }

    @Override
    public void accept(Visitor visitor) {
        visitor.visit(this);
    }
}


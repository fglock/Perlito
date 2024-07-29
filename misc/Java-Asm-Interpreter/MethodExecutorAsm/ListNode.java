import java.util.*;

public class ListNode extends Node {
    List<Node> elements;

    ListNode(List<Node> elements) {
        this.elements = elements;
    }

    @Override
    public String toString() {
        return "List(" + elements + ")";
    }
}


public class TokenizerComparison {
    public static void main(String[] args) {
        String input = "your_large_text_here"; // Replace with actual large text
        int iterations = 1000000;

        // Approach 1: Using charAt
        long startTime1 = System.nanoTime();
        for (int i = 0; i < iterations; i++) {
            for (int j = 0; j < input.length(); j++) {
                char c = input.charAt(j);
            }
        }
        long endTime1 = System.nanoTime();
        System.out.println("Using charAt: " + (endTime1 - startTime1) + " ns");

        // Approach 2: Using char array
        char[] charArray = input.toCharArray();
        long startTime2 = System.nanoTime();
        for (int i = 0; i < iterations; i++) {
            for (int j = 0; j < charArray.length; j++) {
                char c = charArray[j];
            }
        }
        long endTime2 = System.nanoTime();
        System.out.println("Using char array: " + (endTime2 - startTime2) + " ns");
    }
}

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.*;


public class Main {
    static class PageComparator implements Comparator<String> {
        private Set<String> rules;

        public PageComparator(Set<String> rules) {
            this.rules = rules;
        }

        @Override
        public int compare(String x, String y) {
            if (rules.contains(x + "|" + y)) {
                return -1;
            } else if (rules.contains(y + "|" + x)) {
                return 1;
            } else {
                return 0;
            }
        }
    }

    static boolean isCorrectlyOrdered(Set<String> rules, List<String> update) {
        Comparator<String> comparator = new PageComparator(rules);

        for (int i = 0; i < update.size() - 1; i++) {
            String current = update.get(i);
            String next = update.get(i + 1);
            if (comparator.compare(current, next) > 0) {
                return false;
            }
        }

        return true;
    }

    static int middlePageNumber(List<String> update) {
        String middlePage = update.get(update.size() / 2);
        return Integer.parseInt(middlePage);
    }

    static int solvePartOne(Set<String> rules, List<List<String>> updates) {
        return updates.stream()
            .filter(update -> isCorrectlyOrdered(rules, update))
            .mapToInt(update -> middlePageNumber(update))
            .sum();
    }

    static int solvePartTwo(Set<String> rules, List<List<String>> updates) {
        Comparator<String> comparator = new PageComparator(rules);

        return updates.stream()
            .filter(update -> !isCorrectlyOrdered(rules, update))
            .map(update -> update.stream()
                .sorted(comparator)
                .toList())
            .mapToInt(sortedUpdate -> middlePageNumber(sortedUpdate))
            .sum();
    }

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Input file not provided");
            System.exit(1);
        }

        Path inputFile = Path.of(args[0]);
        String input = Files.readString(inputFile);
        String[] sections = input.split("\n\n", 2);

        Set<String> rules = Set.of(sections[0].split("\n"));

        List<List<String>> updates = Arrays.stream(sections[1].split("\n"))
            .map(line -> List.of(line.split(",")))
            .toList();

        System.out.println("--- Day 5: Print Queue ---");
        System.out.println("Answer for part 1: " + solvePartOne(rules, updates));
        System.out.println("Answer for part 2: " + solvePartTwo(rules, updates));
    }
}

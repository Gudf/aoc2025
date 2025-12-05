let day_05_tests = ("Day 5", [
    ("Day 5, part 1: Given example matches", Aoc.Day05.part1, "inputs/day05/example", 3);
    ("Day 5, part 2: Given example matches", Aoc.Day05.part2, "inputs/day05/example", 14)
])

let () = exit (Test.run_tests day_05_tests)

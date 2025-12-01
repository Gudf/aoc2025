let day_01_tests = ("Day 1", [
    ("Day 1, part 1: Given example matches", Aoc.Day01.part1, "inputs/day01/example", 3);
    ("Day 1, part 2: Given example matches", Aoc.Day01.part2, "inputs/day01/example", 6);
    ("Day 1, part 2: Multiple turns at a time", Aoc.Day01.part2, "inputs/day01/multiple", 10)
])

let () = exit (Test.run_tests day_01_tests)

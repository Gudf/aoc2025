let day_01_tests = ("Day 1", [
    ("Day 1, part 1: Given example matches", Aoc.Day01.part1, "inputs/example", 3);
    ("Day 1, part 2: Given example matches", Aoc.Day01.part2, "inputs/example", 6);
    ("Day 1, part 2: Multiple turns at a time", Aoc.Day01.part2, "inputs/multiple", 10)
])

let () = exit (Test.run_tests day_01_tests)

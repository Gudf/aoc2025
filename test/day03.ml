let day_03_tests = ("Day 3", [
    ("Day 3, part 1: Given example matches", Aoc.Day03.part1, "inputs/day03/example", 357);
    ("Day 3, part 2: Given example matches", Aoc.Day03.part2, "inputs/day03/example", 3121910778619)
])

let () = exit (Test.run_tests day_03_tests)

let day_02_tests = ("Day 2", [
    ("Day 2, part 1: Simple test case, 15-2000", Aoc.Day02.part1, "inputs/day02/test", 15129);
    ("Day 2, part 1: Given example matches", Aoc.Day02.part1, "inputs/day02/example", 1227775554);
    ("Day 2, part 2: Given example matches", Aoc.Day02.part2, "inputs/day02/example", 4174379265)
])

let () = exit (Test.run_tests day_02_tests)

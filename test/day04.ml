let day_04_tests = ("Day 4", [
    ("Day 4, part 1: Given example matches", Aoc.Day04.part1, "inputs/day04/example", 13)
(*     ("Day 4, part 2: Given example matches", Aoc.Day04.part2, "inputs/day04/example", 3121910778619) *)
])

let () = exit (Test.run_tests day_04_tests)

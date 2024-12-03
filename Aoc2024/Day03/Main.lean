import Aoc2024.Day03.Solve
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  IO.println "Day 03"
  IO.println ""
  IO.println "Part 1"
  let example1 <- IO.FS.readFile "Aoc2024/Day03/inputs/example1.txt"
  let example2 <- IO.FS.readFile "Aoc2024/Day03/inputs/example2.txt"
  let input <- IO.FS.readFile "Aoc2024/Day03/inputs/input.txt"
  IO.println s!"Example: {solvePart1 example1}"
  IO.println s!"Input: {solvePart1 input}" -- 174960351
  IO.println ""
  IO.println "Part 2"
  IO.println s!"Example: {solvePart2 example2}"
  IO.println s!"Input: {solvePart2 input}" -- 56275602

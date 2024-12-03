import Aoc2024.Day03.Solve
import Aoc2024.CustomMonadLift

def solve (name: String) (inputPath: String) : IO Unit := do
  IO.println name
  let input <- IO.FS.readFile inputPath
  IO.println s!"Part 1: {solvePart1 input}"
  IO.println s!"Part 2: {solvePart2 input}"

def main : IO Unit := do
  IO.println "Day 03"
  IO.println ""
  solve "Example" "Aoc2024/Day03/inputs/example.txt"
  IO.println ""
  solve "Puzzle" "Aoc2024/Day03/inputs/input.txt"

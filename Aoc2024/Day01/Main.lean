import Aoc2024.Day01.Solve
import Aoc2024.CustomMonadLift

def solve (name: String) (inputPath: String) : IO Unit := do
  IO.println name
  let input <- IO.FS.readFile inputPath
  IO.println s!"Part 1: {<- parseAndSolvePart1 input}"
  IO.println s!"Part 2: {<- parseAndSolvePart2 input}"

def main : IO Unit := do
  IO.println "Day 1"
  IO.println ""
  solve "Example" "Aoc2024/Day01/inputs/example.txt"
  IO.println ""
  solve "Puzzle" "Aoc2024/Day01/inputs/input.txt" -- 2066446, 24931009

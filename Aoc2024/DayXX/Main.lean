import Aoc2024.DayXX.Solve
import Aoc2024.CustomMonadLift

def solve (name: String) (inputPath: String) : IO Unit := do
  IO.println name
  let input <- IO.FS.readFile inputPath
  IO.println s!"Part 1: {<- parseAndSolvePart1 input}"
  IO.println s!"Part 2: {<- parseAndSolvePart2 input}"

def main : IO Unit := do
  IO.println "Day XX"
  IO.println ""
  solve "Example" "Aoc2024/DayXX/inputs/example.txt"
  IO.println ""
  solve "Puzzle" "Aoc2024/DayXX/inputs/input.txt"

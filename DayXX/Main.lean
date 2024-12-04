import Aoc2024.DayXX.Solve
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  IO.println "Day XX"
  IO.println ""
  IO.println "Part 1"
  let exampleInput <- IO.FS.readFile "Aoc2024/DayXX/inputs/example.txt"
  let puzzleInput <- IO.FS.readFile "Aoc2024/DayXX/inputs/input.txt"
  IO.println s!"Example: {parseAndSolvePart1 exampleInput}"
  IO.println s!"Puzzle: {parseAndSolvePart1 puzzleInput}" --
  IO.println ""
  IO.println "Part 2"
  IO.println s!"Example: {parseAndSolvePart2 exampleInput}"
  IO.println s!"Puzzle: {parseAndSolvePart2 puzzleInput}" --

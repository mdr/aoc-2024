import Aoc2024.Day04.Solve
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  IO.println "Day 04"
  IO.println ""
  IO.println "Part 1"
  let exampleInput <- IO.FS.readFile "Aoc2024/Day04/inputs/example.txt"
  let puzzleInput <- IO.FS.readFile "Aoc2024/Day04/inputs/input.txt"
  IO.println s!"Example: {solvePart1 exampleInput}"
  IO.println s!"Puzzle: {solvePart1 puzzleInput}" --
  IO.println ""
  IO.println "Part 2"
  IO.println s!"Example: {solvePart2 exampleInput}"
  IO.println s!"Puzzle: {solvePart2 puzzleInput}" --

import Aoc2024.Day08.Solve
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  IO.println "Day 08"
  IO.println ""
  IO.println "Part 1"
  let exampleInput <- IO.FS.readFile "Aoc2024/Day08/inputs/example.txt"
  let puzzleInput <- IO.FS.readFile "Aoc2024/Day08/inputs/input.txt"
  IO.println s!"Example: {parseAndSolvePart1 exampleInput}"
  let answerPart1 := parseAndSolvePart1 puzzleInput
  IO.println s!"Puzzle: {answerPart1}"
  assert! (answerPart1 == 344)
  IO.println ""
  IO.println "Part 2"
  IO.println s!"Example: {parseAndSolvePart2 exampleInput}"
  let answerPart2 := parseAndSolvePart2 puzzleInput
  IO.println s!"Puzzle: {answerPart2}"
  assert! (answerPart2 == 1182)

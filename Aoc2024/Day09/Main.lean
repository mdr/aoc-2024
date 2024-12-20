import Aoc2024.Day09.Solve
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  IO.println "Day 09"
  IO.println ""
  IO.println "Part 1"
  let exampleInput <- IO.FS.readFile "Aoc2024/Day09/inputs/example.txt"
  let puzzleInput <- IO.FS.readFile "Aoc2024/Day09/inputs/input.txt"
  IO.println s!"Example: {<- parseAndSolvePart1 exampleInput}"
  let answerPart1 <- parseAndSolvePart1 puzzleInput
  IO.println s!"Puzzle: {answerPart1}"
  assert! (answerPart1 == 6242766523059)
  IO.println ""
  IO.println "Part 2"
  IO.println s!"Example: {<- parseAndSolvePart2 exampleInput}"
  let answerPart2 <- parseAndSolvePart2 puzzleInput
  IO.println s!"Puzzle: {answerPart2}"
  assert! (answerPart2 == 6272188244509)

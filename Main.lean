import Aoc2024.Day01
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  try
    IO.println "Day 01"
    IO.println ""
    IO.println "Example input"
    let exampleInput <- IO.FS.readFile "inputs/day01/example.txt"
    let examplePart1 <- Aoc2024.Day01.parseAndSolvePart1 exampleInput
    IO.println s!"Part 1: {examplePart1}"
    let examplePart2 <- Aoc2024.Day01.parseAndSolvePart2 exampleInput
    IO.println s!"Part 2: {examplePart2}"
    IO.println ""
    IO.println "Puzzle input"
    let puzzleInput ← IO.FS.readFile "inputs/day01/input.txt"
    let part1 <- Aoc2024.Day01.parseAndSolvePart1 puzzleInput
    IO.println s!"Part 1: {part1}" -- 2066446
    let part2 <- Aoc2024.Day01.parseAndSolvePart2 puzzleInput
    IO.println s!"Part 2: {part2}" -- 24931009
  catch e =>
    IO.println s!"Caught exception: {e}"

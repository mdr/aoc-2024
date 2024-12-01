import Aoc2024.Day01
import Aoc2024.CustomMonadLift

def main : IO Unit := do
  try
    let path := "inputs/day01/input.txt"
    let input ← IO.FS.readFile path
    IO.println "Day 01"
    let part1 <- Aoc2024.Day01.parseAndSolvePart1 input
    IO.println s!"Part 1: {part1}"
    let part2 <- Aoc2024.Day01.parseAndSolvePart2 input
    IO.println s!"Part 2: {part2}"
  catch e =>
    IO.println s!"Caught exception: {e}"

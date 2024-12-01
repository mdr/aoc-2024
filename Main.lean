import Aoc2024.Day01.Solve
import Aoc2024.CustomMonadLift
open Aoc2024.Day01

def solve (name: String) (inputPath: String) : IO Unit := do
  IO.println name
  let input <- IO.FS.readFile inputPath
  let part1 <- parseAndSolvePart1 input
  IO.println s!"Part 1: {part1}"
  let part2 <- parseAndSolvePart2 input
  IO.println s!"Part 2: {part2}"

def main : IO Unit := do
  try
    IO.println "Day 01"
    IO.println ""
    solve "Example" "inputs/day01/example.txt"
    IO.println ""
    solve "Puzzle" "inputs/day01/input.txt" -- 2066446, 24931009
  catch e =>
    IO.println s!"Caught exception: {e}"

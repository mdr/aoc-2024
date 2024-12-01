import Aoc2024.Day01

def main : IO Unit := do
  let path := "inputs/day01/input.txt"
  let inputString ← IO.FS.readFile path
  IO.println "Day 01"
  match Aoc2024.Day01.parseAndSolvePart1 inputString with
  | .ok answer => IO.println s!"Part 1: {answer}"
  | .error e => IO.println s!"Caught exception: {e}"
  match Aoc2024.Day01.parseAndSolvePart2 inputString with
  | .ok answer => IO.println s!"Part 2: {answer}"
  | .error e => IO.println s!"Caught exception: {e}"

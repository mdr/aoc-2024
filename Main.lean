import Aoc2024.Day01

def main : IO Unit := do
  let path := "inputs/day01/input.txt"
  let s ← IO.FS.readFile path
  IO.println "Day 01"
  match Aoc2024.Day01.parseAndSolvePart1 s with
  | some answer => IO.println s!"Part 1: {answer}"
  | none => IO.println "Failed to parse"
  match Aoc2024.Day01.parseAndSolvePart2 s with
  | some answer => IO.println s!"Part 2: {answer}"
  | none => IO.println "Failed to parse"

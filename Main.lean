import Aoc2024.Day01

def liftIO (t : ExceptT String Id α) : IO α := do
  match t with
  | .ok r => EStateM.Result.ok r
  | .error e => EStateM.Result.error e

instance : MonadLift (ExceptT String Id) IO where
  monadLift := liftIO

def main : IO Unit := do
  let path := "inputs/day01/input.txt"
  let input ← IO.FS.readFile path
  IO.println "Day 01"
  try
    let part1 <- Aoc2024.Day01.parseAndSolvePart1 input
    IO.println s!"Part 1: {part1}"
    let part2 <- Aoc2024.Day01.parseAndSolvePart2 input
    IO.println s!"Part 2: {part2}"
  catch e =>
    IO.println s!"Caught exception: {e}"

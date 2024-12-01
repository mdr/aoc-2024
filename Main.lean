import Aoc2024.Day01

def splitOnWhitespace (s : String) : List String :=
  s.split (λ c => c.isWhitespace) |>.filter (· ≠ "")

def parseTwoNumbers (line : String) : Option (Int × Int) :=
  match splitOnWhitespace line with
  | [a, b] =>
    match a.toInt?, b.toInt? with
    | some a, some b => some (a, b)
    | _, _ => none
  | _ => none

#eval parseTwoNumbers "42   17"

def parseLines (s : String) : Option (List (Int × Int)) :=
  s.splitOn "\n" |>.mapM parseTwoNumbers

#eval parseLines "42 17\n1 2\n3 4"

-- theorem test_parseTwoNumbers_valid :
--   parseTwoNumbers "42 17" = some (42, 17) := by
--     rfl

def main : IO Unit := do
  let path := "inputs/day01/input.txt"
  let maybePairs ← IO.FS.readFile path
  match parseLines maybePairs with
  | some pairs => IO.println (Aoc2024.Day01.solvePart2 pairs)
  | none => IO.println "Failed to parse lines"

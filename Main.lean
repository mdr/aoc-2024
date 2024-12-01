def readLines (path : String) : IO (List String) := do
  let contents ← IO.FS.readFile path
  return contents.splitOn "\n"

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

def distance (a b : Int) : Int :=
  (a - b).natAbs

def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1) |>.mergeSort
  let seconds := pairs.map (·.2) |>.mergeSort
  let pairs := firsts.zipWith distance seconds
  sumList pairs

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1)
  let seconds := pairs.map (·.2)
  let countOccurrencesInSeconds (n : Int) : Int :=
    seconds.filter (· == n) |>.length
  let similarityScore (n : Int) : Int :=
    countOccurrencesInSeconds n * n
  firsts.map similarityScore |> sumList

def main : IO Unit := do
  let path := "inputs/day01/input.txt"
  let maybePairs ← IO.FS.readFile path
  match parseLines maybePairs with
  | some pairs => IO.println (solvePart2 pairs)
  | none => IO.println "Failed to parse lines"

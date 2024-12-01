namespace Aoc2024.Day01

def splitOnWhitespace (s : String) : List String :=
  s.split (·.isWhitespace) |>.filter (· ≠ "")

def parseTwoNumbers (line : String) :  Except String (Int × Int) :=
  match splitOnWhitespace line with
  | [a, b] =>
    match a.toInt?, b.toInt? with
    | some a, some b => pure (a, b)
    | _, _ => throw "Failed to parse numbers"
  | _ => throw "Failed to parse line"

def parseLines (s : String) : Except String (List (Int × Int)) :=
  s.splitOn "\n" |>.mapM parseTwoNumbers

def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1) |>.mergeSort
  let seconds := pairs.map (·.2) |>.mergeSort
  let distance (a b : Int) : Int := (a - b).natAbs
  let pairs := firsts.zipWith distance seconds
  sumList pairs

def parseAndSolvePart1 (s : String) : Except String Int :=
  parseLines s |>.map solvePart1

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1)
  let seconds := pairs.map (·.2)
  let countOccurrencesInSeconds (n : Int) : Int :=
    seconds.filter (· == n) |>.length
  let similarityScore (n : Int) : Int :=
    countOccurrencesInSeconds n * n
  firsts.map similarityScore |> sumList

def parseAndSolvePart2 (s : String) : Except String Int :=
  parseLines s |>.map solvePart2

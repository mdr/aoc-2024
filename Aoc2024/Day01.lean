namespace Aoc2024.Day01

def splitOnWhitespace (s : String) : List String :=
  s.split Char.isWhitespace |> .filter (· ≠ "")

def parseNumber (s: String) : Except String Int :=
  match s.toInt? with
  | some n => pure n
  | none => throw s!"Failed to parse number: {s}"

def getTwoElements (l : List α) : Except String (α × α) :=
  match l with
  | [a, b] => pure (a, b)
  | _ => throw s!"Expected two elements, got {l.length}"

def parseTwoNumbers (line : String) : Except String (Int × Int) := do
  let (a, b) ← getTwoElements (splitOnWhitespace line)
  Prod.mk <$> parseNumber a <*> parseNumber b

def parseLines (s : String) : Except String (List (Int × Int)) :=
  s.splitOn "\n" |>.mapM parseTwoNumbers

def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let distance (a b : Int) : Int := (a - b).natAbs
  (firsts.mergeSort.zipWith distance seconds.mergeSort) |> sumList

def parseAndSolvePart1 (s : String) : Except String Int :=
  s |> parseLines |> .map solvePart1

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let similarityScore (n : Int) : Int := seconds.filter (· == n) |>.length |> (· * n)
  firsts.map similarityScore |> sumList

def parseAndSolvePart2 (s : String) : Except String Int :=
  s |> parseLines |>.map solvePart2

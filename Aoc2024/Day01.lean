namespace Aoc2024.Day01

def splitOnWhitespace (s : String) : List String :=
  s.split Char.isWhitespace |> .filter (· ≠ "")

def getOrExcept (message : String) (o : Option α) : Except String α :=
  match o with
  | some x => pure x
  | none => throw message

def parseNumber (s: String) : Except String Int :=
  s.toInt? |> getOrExcept s!"Failed to parse as number: {s}"

def getTwoElements (l : List α) : Except String (α × α) := do
  let [a, b] := l | throw s!"Expected two elements, but got {l.length}"
  pure (a, b)

def parseTwoNumbers (line : String) : Except String (Int × Int) := do
  let (a, b) ← getTwoElements (splitOnWhitespace line)
  Prod.mk <$> parseNumber a <*> parseNumber b

def parseLines (s : String) : Except String (List (Int × Int)) :=
  s.splitOn "\n" |>.mapM parseTwoNumbers

def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

def sumListBy (f : Int → Int) (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + f x) 0

theorem testSumListEmpty : sumList [] = 0 := rfl
theorem testSumList : sumList [1, 2, 3] = 6 := rfl

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let distance (a b : Int) : Int := (a - b).natAbs
  (firsts.mergeSort.zipWith distance seconds.mergeSort) |> sumList

def parseAndSolvePart1 (s : String) : Except String Int :=
  s |> parseLines |> .map solvePart1

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let similarityScore (n : Int) : Int := seconds.count n * n
  firsts |> sumListBy similarityScore

def parseAndSolvePart2 (s : String) : Except String Int :=
  s |> parseLines |>.map solvePart2

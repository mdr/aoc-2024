import Aoc2024.Utils

def splitOnWhitespace (s : String) : List String :=
  s.split Char.isWhitespace |> .filter (· ≠ "")

def parseNumber (s: String) : Except String Int :=
  s.toInt? |> getOrThrow s!"Failed to parse as number: {s}"

def getTwoElements (l : List α) : Except String (α × α) := do
  let [a, b] := l | throw s!"Expected two elements, but got {l.length}"
  pure (a, b)

def parseTwoNumbers (line : String) : Except String (Int × Int) := do
  let (a, b) ← getTwoElements (splitOnWhitespace line)
  Prod.mk <$> parseNumber a <*> parseNumber b

def parseLines (s : String) : Except String (List (Int × Int)) :=
  s.splitOn "\n" |>.mapM parseTwoNumbers

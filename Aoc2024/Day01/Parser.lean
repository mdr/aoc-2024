import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

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

def lineParser : Parser (Int × Int) := do
  let a ← String.digits
  let _ <- String.ws
  let b ← String.digits
  pure (a, b)

-- def inputParser : Parser (List (Int × Int)) := do
--   let lines ← lineParser.many1
--   pure lines

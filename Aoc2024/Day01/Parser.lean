import Aoc2024.Utils
import Aoc2024.Day01.Examples
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def lineParser : Parser (Int × Int) := do
  let a ← String.digits
  let _ <- String.ws
  let b ← String.digits
  pure (a, b)

private def inputParser : Parser (List (Int × Int)) := sepBy lineParser (String.skipChar '\n')

def parseLines : String -> Except String (List (Int × Int)) := inputParser.run

#guard parseLines exampleInput == Except.ok [
  (3, 4),
  (4, 3),
  (2, 5),
  (1, 3),
  (3, 9),
  (3, 3)
]

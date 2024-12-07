import Aoc2024.Day07.Examples
import Aoc2024.Day07.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def number : Parser Int := do pure (<- digits)

private def equation : Parser Equation := do
  let testValue <- number
  let _ ← skipString ": "
  let numbers <- sepBy number (skipChar ' ')
  pure { testValue, numbers }

private def equations : Parser (List Equation) := sepBy equation (skipChar '\n')

def parseEquations : String -> Except String (List Equation) := equations.run

#guard parseEquations "190: 10 19\n3267: 81 40 27" == Except.ok [
  { testValue := 190, numbers := [10, 19] },
  { testValue := 3267, numbers := [81, 40, 27] }
]

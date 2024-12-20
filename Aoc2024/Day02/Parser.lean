import Aoc2024.Utils
import Aoc2024.Day02.Types
import Aoc2024.Day02.Examples
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def reportParser : Parser Report := sepBy String.digits (String.skipChar ' ')

private def inputParser : Parser (List Report) := sepBy reportParser (String.skipChar '\n')

def parseReports : String -> Except String (List Report) := inputParser.run

#guard parseReports exampleInput == Except.ok [
  [7, 6, 4, 2, 1],
  [1, 2, 7, 8, 9],
  [9, 7, 6, 2, 1],
  [1, 3, 2, 4, 5],
  [8, 6, 4, 4, 1],
  [1, 3, 6, 7, 9]
]

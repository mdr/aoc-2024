import Aoc2024.Day03.Examples
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def inputParser : Parser (List Int) := sorry

def parseThings : String -> Except String (List Int) := inputParser.run

-- #guard parseReports exampleInput == Except.ok 42

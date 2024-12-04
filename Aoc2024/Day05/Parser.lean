import Aoc2024.Day05.Examples
import Aoc2024.Day05.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def inputParser : Parser (List Int) := sorry

def parseThings : String -> Except String (List Int) := inputParser.run

-- #guard parseReports exampleInput == Except.ok 42

import Aoc2024.DayXX.Examples
import Aoc2024.DayXX.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def inputParser : Parser (List Int) := sorry

def parseInput : String -> Except String (List Int) := inputParser.run

-- #guard parseInput exampleInput == Except.ok 42

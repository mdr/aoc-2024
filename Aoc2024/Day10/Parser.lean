import Aoc2024.Day10.Examples
import Aoc2024.Day10.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def inputParser : Parser (List Int) := sorry

def parseInput : String -> Except String (List Int) := inputParser.run

-- #guard parseInput exampleInput == Except.ok 42

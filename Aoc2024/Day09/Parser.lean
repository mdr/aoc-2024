import Aoc2024.Day09.Examples
import Aoc2024.Day09.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def diskMapParser : Parser (List Nat) := Array.toList <$> many (charDigitToNat <$> digit)

def parseDiskMap : String -> Except String (List Nat) := diskMapParser.run

#guard parseDiskMap exampleInput == Except.ok [2, 3, 3, 3, 1, 3, 3, 1, 2, 1, 4, 1, 4, 1, 3, 1, 4, 0, 2]

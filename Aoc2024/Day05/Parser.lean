import Aoc2024.Day05.Examples
import Aoc2024.Day05.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

private def number : Parser Int := do
  let nat <- digits
  pure nat

private def orderingRuleParser : Parser OrderingRule := do
  let before ← number
  skipChar '|'
  let after ← number
  pure { before, after }

private def updateParser : Parser Update := do
  let pages <- sepBy number (skipChar ',')
  pure { pages }

private def inputParser : Parser PuzzleInput := do
  let rules ← many (orderingRuleParser <* skipChar '\n')
  skipChar '\n'
  let updates ← sepBy updateParser (skipChar '\n')
  pure { rules := rules.toList, updates }

def parseInput : String -> Except String PuzzleInput := inputParser.run

#guard parseInput "1|2\n\n1,2\n3,4" == Except.ok {
  rules := [{ before := 1, after := 2 }],
  updates := [{ pages := [1, 2] }, { pages := [3, 4] }]
}
#guard parseInput exampleInput |>.isOk

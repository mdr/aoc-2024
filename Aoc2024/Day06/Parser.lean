import Aoc2024.Day06.Examples
import Aoc2024.Day06.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std (HashSet)

-- private def inputParser : Parser (List Int) := sorry

private def decorateWithCoordinates (s: String): List (Point × Char) :=
  s.splitOn "\n" |>.enum |>.bind (λ ⟨y, line⟩ =>
    line.toList.enum.map (λ ⟨x, c⟩ => (⟨x, y⟩, c))
  )

def parseInput (s: String): Except String PuzzleInput := do
  let decoratedChars := decorateWithCoordinates s
  let obstacles := decoratedChars.filterMap (λ ⟨p, c⟩ => (c == '#').toOption p) |>.toSet
  let start <- decoratedChars.filterMap (λ ⟨p, c⟩ => (c == '^').toOption p) |>.head? |>.getOrThrow "no start found"
  let (xs, ys) := decoratedChars.map (·.1.toPair) |>.unzip
  let width <- xs.toArray.max?.map (· + 1 |>.toNat) |>.getOrThrow "empty input"
  let height <- ys.toArray.max?.map (· + 1 |>.toNat) |>.getOrThrow "empty input"
  pure { obstacles, start, width, height }

-- #guard parseReports exampleInput == Except.ok 42

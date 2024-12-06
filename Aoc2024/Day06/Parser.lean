import Aoc2024.Day06.Examples
import Aoc2024.Day06.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std (HashSet)

instance : Bind List where bind := List.bind
instance : Pure List where pure := List.pure

private def decorateWithCoordinates (s: String): List (Point × Char) := do
  let (y, line) <- s.splitOn "\n" |> .enum
  let (x, c) <- line.toList.enum
  pure (⟨x, y⟩, c)

def parseInput (s: String): Except String PuzzleInput := do
  let decoratedChars := decorateWithCoordinates s
  let obstacles := decoratedChars.filterMap (λ ⟨p, c⟩ => (c == '#').toOption p) |>.toSet
  let start <- decoratedChars.filterMap (λ ⟨p, c⟩ => (c == '^').toOption p) |>.head? |>.getOrThrow "no start found"
  let (xs, ys) := decoratedChars.map (·.1.toPair) |>.unzip
  let width <- xs.max?.map (· + 1 |>.toNat) |>.getOrThrow "empty input"
  let height <- ys.max?.map (· + 1 |>.toNat) |>.getOrThrow "empty input"
  let bounds: Rectangle := { topLeft := Point.origin, width, height }
  pure { obstacles, start, bounds }

private def examplePuzzleInput := parseInput exampleInput
#guard (examplePuzzleInput.map (·.bounds)) == Except.ok (Rectangle.mk Point.origin 10 10)
#guard (examplePuzzleInput.map (·.start)) == Except.ok ⟨4, 6⟩
#guard (examplePuzzleInput.map (·.obstacles.size)) == Except.ok 8

import Aoc2024.Day08.Examples
import Aoc2024.Day08.Types
import Aoc2024.Utils
import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec

instance : Bind List where bind := List.bind
instance : Pure List where pure := List.pure

private def decorateWithCoordinates (s: String): List (Point × Char) := do
  let (y, line) <- s.splitOn "\n" |> .enum
  let (x, c) <- line.toList.enum
  pure (⟨x, y⟩, c)

def parseInput (s : String): PuzzleInput :=
  let decoratedChars := decorateWithCoordinates s
  let (xs, ys) := decoratedChars.map (·.1.toPair) |>.unzip
  let width := xs.max?.map (· + 1 |>.toNat) |>.getD 0
  let height := ys.max?.map (· + 1 |>.toNat) |>.getD 0
  let bounds: Rectangle := { topLeft := Point.origin, width, height }
  { bounds, decoratedChars }
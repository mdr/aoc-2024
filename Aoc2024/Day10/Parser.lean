import Aoc2024.Day10.Examples
import Aoc2024.Day10.Types
import Aoc2024.Utils
import Std
open Std (HashMap)

private def decorateWithCoordinates (s: String): List (Point × Char) := do
  let (y, line) <- s.splitOn "\n" |> .enum
  let (x, c) <- line.toList.enum
  pure ⟨⟨x, y⟩, c⟩

def parseHeightMap (s : String): PuzzleInput :=
  let decoratedChars := decorateWithCoordinates s
  let (xs, ys) := decoratedChars.map (·.1.toPair) |>.unzip
  let width := xs.max?.map (· + 1 |>.toNat) |>.getD 0
  let height := ys.max?.map (· + 1 |>.toNat) |>.getD 0
  let bounds: Rectangle := { topLeft := Point.origin, width, height }
  let heights: HashMap Point Height :=
    decoratedChars.foldl (init := HashMap.empty) λ m ⟨p, c⟩ => m.insert p (charDigitToNat c)
  { bounds, heights }

import Aoc2024.Utils
import Std
open Std (HashSet)

structure PointAndChar where
  point: Point
  char: Char
deriving Repr, Inhabited, BEq, Hashable

structure PuzzleInput where
  bounds : Rectangle
  decoratedChars : List PointAndChar
deriving Repr, Inhabited, BEq, Hashable

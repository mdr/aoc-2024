import Aoc2024.Utils
import Std
open Std (HashMap)

abbrev Height := Nat

structure PuzzleInput where
  bounds : Rectangle
  heights : HashMap Point Height
deriving Repr, Inhabited

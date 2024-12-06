import Std
open Std (HashSet)

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable, Repr

def Point.toPair (p : Point) : (Int × Int) := (p.x, p.y)

inductive Direction where
  | Up
  | Right
  | Down
  | Left

structure PuzzleInput where
  obstacles : HashSet Point
  width : Nat
  height : Nat
  start : Point
deriving Repr

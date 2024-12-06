import Std
open Std (HashSet)

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable, Repr, Inhabited

def Point.toPair (p : Point) : (Int × Int) := (p.x, p.y)

inductive Direction where
  | Up
  | Right
  | Down
  | Left
deriving BEq, Repr, Inhabited, Hashable

def Direction.turnRight : Direction -> Direction
  | Direction.Up => Direction.Right
  | Direction.Right => Direction.Down
  | Direction.Down => Direction.Left
  | Direction.Left => Direction.Up

def Point.step (p : Point) (d : Direction) : Point :=
  match d with
  | Direction.Up => { p with y := p.y - 1 }
  | Direction.Right => { p with x := p.x + 1 }
  | Direction.Down => { p with y := p.y + 1 }
  | Direction.Left => { p with x := p.x - 1 }

def Point.origin : Point := { x := 0, y := 0 }

structure Rectangle where
  topLeft : Point
  width: Nat
  height: Nat
deriving Repr, BEq, Hashable, Inhabited

def Rectangle.contains (r : Rectangle) (p : Point) : Bool :=
  r.topLeft.x ≤ p.x && p.x < r.topLeft.x + r.width &&
    r.topLeft.y ≤ p.y && p.y < r.topLeft.y + r.height

#guard Rectangle.contains { topLeft := Point.origin, width := 2, height := 2 } { x := 1, y := 1 }
#guard !Rectangle.contains { topLeft := Point.origin, width := 2, height := 2 } { x := 2, y := 2 }

structure PuzzleInput where
  obstacles : HashSet Point
  bounds : Rectangle
  start : Point
deriving Repr

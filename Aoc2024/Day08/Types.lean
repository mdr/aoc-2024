import Aoc2024.Utils
import Std
open Std (HashSet)

structure Point where
  x : Int
  y : Int
deriving BEq, Hashable, Repr, Inhabited

def Point.toPair (p : Point) : (Int × Int) := (p.x, p.y)

def Point.origin : Point := { x := 0, y := 0 }

structure Vector where
  x : Int
  y : Int
deriving BEq, Hashable, Repr, Inhabited

def Point.vectorTo (from_ to : Point) : Vector := { x := to.x - from_.x, y := to.y - from_.y }
#guard ({ x := 1, y := 1 } : Point).vectorTo { x := 2, y := 2 } == { x := 1, y := 1 }

def Point.add (p : Point) (v : Vector) : Point := { x := p.x + v.x, y := p.y + v.y }
#guard Point.add { x := 1, y := 1 } { x := 2, y := 2 } == { x := 3, y := 3 }

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

def Rectangle.allPoints (r : Rectangle) : List Point := do
  let x <- intRange r.topLeft.x (r.topLeft.x + r.width)
  let y <- intRange r.topLeft.y (r.topLeft.y + r.height)
  return { x := x, y := y }

#guard Rectangle.allPoints { topLeft := Point.origin, width := 2, height := 2 } ==
  [{ x := 0, y := 0 }, { x := 0, y := 1 }, { x := 1, y := 0 }, { x := 1, y := 1 }]

structure PointAndChar where
  point: Point
  char: Char
deriving Repr, Inhabited, BEq, Hashable

structure PuzzleInput where
  bounds : Rectangle
  decoratedChars : List PointAndChar
deriving Repr, Inhabited, BEq, Hashable

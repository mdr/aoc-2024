import Std
import Aoc2024.Utils
open Std (HashSet)

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

structure PuzzleInput where
  obstacles : HashSet Point
  bounds : Rectangle
  start : Point
deriving Repr

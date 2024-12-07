structure Equation where
  testValue : Int
  numbers : List Int
deriving Repr, BEq, Hashable, Inhabited

inductive Operator where
  | add
  | multiply
  | concatenation
deriving Repr, BEq, Hashable, Inhabited

private def concatenate (a : Int) (b : Int) : Int := (reprStr a ++ reprStr b).toInt!

def Operator.fn : Operator -> Int -> Int -> Int
  | .add, a, b => a + b
  | .multiply, a, b => a * b
  | .concatenation, a, b => concatenate a b

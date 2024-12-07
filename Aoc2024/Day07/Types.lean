structure Equation where
  testValue : Int
  numbers : List Int
deriving Repr, BEq, Hashable, Inhabited

inductive Operator where
  | add
  | multiply
deriving Repr, BEq, Hashable, Inhabited

def Operator.all : List Operator := [Operator.add, Operator.multiply]

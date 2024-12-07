structure Equation where
  testValue : Int
  numbers : List Int
deriving Repr, BEq, Hashable, Inhabited

inductive Operator where
  | add
  | multiply
  | concatenation
deriving Repr, BEq, Hashable, Inhabited

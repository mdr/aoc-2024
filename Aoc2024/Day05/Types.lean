abbrev Page := Int

structure OrderingRule where
  before : Page
  after : Page
deriving BEq, Hashable, Repr

def OrderingRule.toPair (r : OrderingRule) : Page × Page :=
  (r.before, r.after)

structure Update where
  pages : List Page
deriving BEq, Hashable, Repr

structure PuzzleInput where
  rules : List OrderingRule
  updates : List Update
deriving BEq, Hashable, Repr

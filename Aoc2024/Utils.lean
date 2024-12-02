import Std
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std (HashSet)

-- https://brandonrozek.com/blog/writing-unit-tests-lean-4/
def Except.deq [DecidableEq α] [DecidableEq β] : DecidableEq (Except α β) := by
  unfold DecidableEq
  intro a b
  cases a <;> cases b <;>
  -- Get rid of obvious cases where .ok != .err
  try { apply isFalse ; intro h ; injection h }
  case error.error c d =>
    match decEq c d with
      | isTrue h => apply isTrue (by rw [h])
      | isFalse _ => apply isFalse (by intro h; injection h; contradiction)
  case ok.ok c d =>
    match decEq c d with
      | isTrue h => apply isTrue (by rw [h])
      | isFalse _ => apply isFalse (by intro h; injection h; contradiction)

instance: DecidableEq (Except String Bool) := Except.deq

instance [BEq α] [BEq β] : BEq (Except α β) where
  beq := λ x y => match x, y with
    | Except.ok x, Except.ok y => x == y
    | Except.error x, Except.error y => x == y
    | _, _ => false

def getOrThrow (message : String) : Option α -> Except String α
  | some x => pure x
  | none => throw message

#guard (getOrThrow "Error" (some 42) == Except.ok 42)
#guard (getOrThrow "Error" (none : Option Int) == Except.error "Error")

def sepBy (p : Parser α) (sep : Parser β) : Parser (List α) :=
  (do
    let x ← p
    let xs ← many (sep *> p)
    pure (x :: xs.toList)
  ) <|> pure []

#guard (sepBy digits (skipChar ',')).run "1,2,3" == .ok [1, 2, 3]
#guard (sepBy digits (skipChar ',')).run "4" == .ok [4]
#guard (sepBy digits (skipChar ',')).run "" == .ok []

namespace List

  def sum (xs: List Int) : Int := xs.foldl .add 0

  def sumBy (f : α → Int) (xs : List α) : Int := xs.foldl (λ acc x => acc + f x) 0

  def toSet {α:Type} [BEq α] [Hashable α] (xs: List α) : HashSet α :=
    HashSet.ofList xs

  def differences (xs: List Int) : List Int :=
    xs.zip xs.tail |>.map (λ (a, b) => b - a)

end List

#guard [].sum = 0
#guard [1, 2, 3].sum = 6

#guard [1, 2, 3].sumBy (λ x => x * x) = 14

#guard [].differences == []
#guard [1].differences == []
#guard [1, 2, 3, 2, 5].differences == [1, 1, -1, 3]

namespace Std.HashSet

  def isSubsetOf [Hashable α] [BEq α] (xs: HashSet α) (ys: HashSet α) : Bool :=
    xs.all ys.contains

end Std.HashSet

#guard [1, 3].toSet.isSubsetOf [1, 2, 3].toSet == true
#guard [1, 2, 3].toSet.isSubsetOf [1, 3].toSet == false

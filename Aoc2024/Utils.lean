import Std
import Batteries
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

  #guard [].sum = 0
  #guard [1, 2, 3].sum = 6

  def sumBy (f : α → Int) (xs : List α) : Int := xs.foldl (λ acc x => acc + f x) 0

  #guard [1, 2, 3].sumBy (λ x => x * x) = 14

  def toSet {α:Type} [BEq α] [Hashable α] (xs: List α) : HashSet α :=
    HashSet.ofList xs

  def differences (xs: List Int) : List Int :=
    xs.zip xs.tail |>.map (λ (a, b) => b - a)

  #guard [].differences == []
  #guard [1].differences == []
  #guard [1, 2, 3, 2, 5].differences == [1, 1, -1, 3]

  def slidingWindows (n : Nat) (xs : List α) : List (List α) :=
    match xs with
      | [] => []
      | (x :: xs) =>
        let window := x :: xs.take (n - 1)
        if window.length < n then []
        else window :: slidingWindows n xs

  #guard [1, 2, 3, 4, 5].slidingWindows 2 = [[1, 2], [2, 3], [3, 4], [4, 5]]
  #guard [1, 2, 3, 4, 5].slidingWindows 1 = [[1], [2], [3], [4], [5]]
  #guard [1, 2, 3, 4, 5].slidingWindows 0 = [[1], [2], [3], [4], [5]]
  #guard [1].slidingWindows 2 = []
  #guard ([]: List Nat).slidingWindows 2 = []

  -- ported from https://hackage.haskell.org/package/universe-base-1.1.4/docs/src/Data.Universe.Helpers.html#diagonals
  def diagonals (grid : List (List α)) : List (List α) :=
    let rec go (b : List (List α)) (es : List (List α)) : List (List α) :=
      let diagonal := b.filterMap List.head?
      let ts := b.filterMap List.tail?
      diagonal :: match es with
        | [] => ts.transpose
        | e :: es' => go (e :: ts) es'
    (go [] grid).tail!

  #guard [
    ["A", "B", "C", "D"],
    ["E", "F", "G", "H"],
    ["I", "J", "K", "L"],
  ].diagonals = [
    ["A"],
    ["E", "B"],
    ["I", "F", "C"],
    ["J", "G", "D"],
    ["K", "H"],
    ["L"],
  ]
end List

namespace Std.HashSet

  def isSubsetOf [Hashable α] [BEq α] (xs: HashSet α) (ys: HashSet α) : Bool :=
    xs.all ys.contains

end Std.HashSet

#guard [1, 3].toSet.isSubsetOf [1, 2, 3].toSet == true
#guard [1, 2, 3].toSet.isSubsetOf [1, 3].toSet == false

namespace String
  def lines (s : String) : List String := s.splitOn "\n"
end String

#guard "1\n2\n3".lines == ["1", "2", "3"]

def iterate (n : Nat) (f : α -> α) (x : α) : α :=
  match n with
  | 0 => x
  | n + 1 => iterate n f (f x)

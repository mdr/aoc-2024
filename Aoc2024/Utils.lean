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

instance listBind: Bind List where bind := List.bind
instance listPure: Pure List where pure := List.pure

namespace List

  def sum (xs: List Int) : Int := xs.foldl .add 0

  #guard [].sum = 0
  #guard [1, 2, 3].sum = 6

  def sumBy (f : α → Int) (xs : List α) : Int := xs.foldl (λ acc x => acc + f x) 0

  #guard [1, 2, 3].sumBy (λ x => x * x) = 14

  def toSet {α:Type} [BEq α] [Hashable α] (xs: List α) : HashSet α :=
    HashSet.ofList xs

  def slidingPairs (xs: List α) : List (α × α) :=
    xs.zip xs.tail

  #guard [1, 2, 3, 4].slidingPairs == [(1, 2), (2, 3), (3, 4)]

  def differences (xs: List Int) : List Int :=
    xs.slidingPairs |>.map (λ (a, b) => b - a)

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
  #guard ([] : List (List Nat)).diagonals = []
  #guard ([["A"]]).diagonals = [["A"]]
  #guard ([["A", "B"]]).diagonals = [["A"], ["B"]]
  #guard [
    ["A", "B", "C"],
    ["D"],
    ["E", "F"],
  ].diagonals = [
    ["A"],
    ["D", "B"],
    ["E", "C"],
    ["F"],
  ]

  def filterNot (p : α -> Bool) (xs : List α) : List α :=
    xs.filter (λ x => !p x)

  #guard [1, 2, 3, 4].filterNot (λ x => x % 2 == 0) == [1, 3]

  def maxBy (f : α -> Nat) (xs : List α) : Option α :=
    xs.foldl (λ acc x => match acc with
      | some y => if f x > f y then some x else some y
      | none => some x
    ) none

  #guard [1, 2, 3, 4].maxBy id == some 4

  def replicateM (n : Nat) (options : List α) : List (List α) :=
    let rec loop (n : Nat) (acc : List (List α)) : List (List α) :=
      match n with
      | 0 => acc
      | n + 1 => loop n (acc.bind λ l => options.map (λ o => o :: l))
    loop n [[]]

  #guard [1, 2].replicateM 0 == [[]]
  #guard [1, 2].replicateM 3 == [[1, 1, 1], [2, 1, 1], [1, 2, 1], [2, 2, 1], [1, 1, 2], [2, 1, 2], [1, 2, 2], [2, 2, 2]]

  def eraseAll [BEq α] (x: α) (xs : List α)  : List α := xs.filter (· != x)
  #guard [1, 2, 3, 2].eraseAll 2 == [1, 3]

  def combinations {α} [BEq α] (n : Nat) (xs : List α) : List (List α) :=
    match n with
      | 0 => [[]]
      | n + 1 =>
        match xs with
          | [] => []
          | x :: xs => (xs.combinations n).map (x :: ·) ++ xs.combinations (n + 1)

  #guard [1, 2, 3].combinations 0 == [[]]
  #guard [1, 2, 3].combinations 1 == [[1], [2], [3]]
  #guard [1, 2, 3].combinations 2 == [[1, 2], [1, 3], [2, 3]]
  #guard [1, 2, 3].combinations 3 == [[1, 2, 3]]
  #guard [1, 2, 3].combinations 4 == []
  #guard [1, 2, 3, 4].combinations 2 == [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]]

      -- xs.bind (λ x => (xs.filter (· != x)).combinations n.map (x :: ·))

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

namespace Bool
  def toOption (x : Bool) (y : α) : Option α := if x then some y else none

  #guard true.toOption 42 == some 42
  #guard false.toOption 42 == none
end Bool

namespace Option
  def getOrThrow (message : String) : Option α -> Except String α
    | some x => pure x
    | none => throw message
  #guard (some 42).getOrThrow "Error" == Except.ok 42
  #guard (none : Option Int).getOrThrow "Error" == Except.error "Error"
end Option

def natRange (from_ to : Nat) : List Nat :=
  let ns := List.range (to - from_)
  ns.map (· + from_)
#guard natRange 0 3 == [0, 1, 2]
#guard natRange 1 4 == [1, 2, 3]

def intRange (m n : Int) : List Int :=
  ((List.range (Int.toNat (n - m))) : List Nat).map fun (r : Nat) => (m + r : Int)
#guard intRange 0 3 == [0, 1, 2]
#guard intRange (-3) 2 == [-3, -2, -1, 0, 1]
#guard intRange 1 1 == []

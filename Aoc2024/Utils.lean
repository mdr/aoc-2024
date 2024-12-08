import Std
import Batteries
open Std.Internal.Parsec.String
open Std.Internal.Parsec
open Std (HashSet HashMap)

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

  def groupByKey [BEq α] [Hashable α] (key : β → α) (xs : List β): HashMap α (List β) :=
     xs.toArray.groupByKey key |>.map (λ _ v => v.toList)

  def groupByAndTransformValues [BEq α] [Hashable α] (key : β → α) (f : β → γ) (xs : List β): HashMap α (List γ) :=
    xs.groupByKey key |>.map (λ _ v => v.map f)

  def flatMap {α : Type u} {β : Type v} (a : List α) (b : α → List β) : List β := join (map b a)

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

def intRange (from_ to : Int) : List Int := (List.range (to - from_).toNat).map (λ (n : Nat) => (from_ + n))
#guard intRange 0 3 == [0, 1, 2]
#guard intRange (-3) 2 == [-3, -2, -1, 0, 1]
#guard intRange 1 1 == []

instance hashableChar: Hashable Char where hash c := c.toNat |> hash

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

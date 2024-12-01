def sumList (nums : List Int) : Int := nums.foldl (· + ·) 0

#guard (sumList [] == 0)
#guard (sumList [1, 2, 3] == 6)

def sumListBy (f : Int → Int) (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + f x) 0

#guard (sumListBy (fun x => x * x) [1, 2, 3] == 14)

instance [BEq α] : BEq (Except String α) where
  beq := fun x y => match x, y with
    | Except.ok x, Except.ok y => x == y
    | Except.error x, Except.error y => x == y
    | _, _ => false

def getOrExcept (message : String) (o : Option α) : Except String α :=
  match o with
  | some x => pure x
  | none => throw message

#guard (getOrExcept "Error" (some 42) == Except.ok 42)
#guard (getOrExcept "Error" (none : Option Int) == Except.error "Error")

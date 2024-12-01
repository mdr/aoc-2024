def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

theorem testSumListEmpty : sumList [] = 0 := rfl
theorem testSumList : sumList [1, 2, 3] = 6 := rfl

def sumListBy (f : Int → Int) (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + f x) 0

def getOrExcept (message : String) (o : Option α) : Except String α :=
  match o with
  | some x => pure x
  | none => throw message

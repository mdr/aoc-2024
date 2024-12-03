inductive Instruction where
  | mul (n m : Nat) : Instruction
  | do : Instruction
  | dont: Instruction
deriving instance BEq, Hashable, Repr for Instruction

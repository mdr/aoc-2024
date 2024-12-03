import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Regex
open Regex (Match Captures)

inductive Instruction where
  | mul (n m : Nat) : Instruction
  | do : Instruction
  | dont: Instruction
deriving instance BEq, Hashable, Repr for Instruction

def instructionRegex: Regex := regex% r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"

def matchToNat (m : Match) : Option Nat := m.toNat?

def optMatchToNat (m : Option Match) : Option Nat := m >>= matchToNat

def matchesToInstruction (ms : Array (Option Match)): Option Instruction :=
  match ms.toList with
  | [some m1, some m2] => Instruction.mul <$> matchToNat m1 <*> matchToNat m2
  | _ => none

def capturesToInstruction (captures: Captures) : Option Instruction :=
  if captures.fullMatch == "do()" then some Instruction.do
  else if captures.fullMatch == "don't()" then some Instruction.dont
  else matchesToInstruction captures.groups

def findInstructions (s : String) : List Instruction :=
  Regex.all_captures s instructionRegex |>.toList |>.bind (fun captures => captures |> capturesToInstruction |> Option.toList)

#guard findInstructions "mul(1,2) mul(3,4)" == [Instruction.mul 1 2, Instruction.mul 3 4]
#guard findInstructions "do()" == [Instruction.do]
#guard findInstructions "don't()" == [Instruction.dont]

def solvePart1 (s : String) : Int := findInstructions s |>.sumBy (fun
  | Instruction.mul n m => n * m
  | _ => 1
)

#guard solvePart1 exampleInput = 161

private def filterInstructions (enabled : Bool) : List Instruction -> List Instruction
  | List.nil => []
  | List.cons (Instruction.mul m n) rest =>
    if enabled then List.cons (Instruction.mul m n) (filterInstructions enabled rest)
   else filterInstructions enabled rest
  | List.cons Instruction.do rest => filterInstructions true rest
  | List.cons Instruction.dont rest => filterInstructions false rest

#eval findInstructions "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" |> filterInstructions true

def solvePart2 (s : String) : Int := s |> findInstructions |> filterInstructions true |> List.sumBy (fun
  | Instruction.mul n m => n * m
  | _ => 1
)

#eval solvePart2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

#guard solvePart2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" == 48


-- #guard parseAndSolvePart2 exampleInput == Except.ok 4

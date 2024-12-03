import Aoc2024.Day03.Examples
import Aoc2024.Day03.Types
import Std
import Regex
open Regex (Match Captures)

def instructionRegex: Regex := regex% r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)"

private def matchToMultiplyInstruction (captures: Captures): Option Instruction :=
  match captures.groups.toList with
  | [some m1, some m2] => Instruction.mul <$> m1.toNat? <*> m2.toNat?
  | _ => none

def matchToInstruction (captures: Captures) : Option Instruction :=
  if captures.fullMatch == "do()" then some Instruction.do
  else if captures.fullMatch == "don't()" then some Instruction.dont
  else matchToMultiplyInstruction captures

def findInstructions (s : String) : List Instruction :=
  Regex.all_captures s instructionRegex |>.toList |>.bind (· |> matchToInstruction |> Option.toList)

#guard findInstructions "mul(1,2) mul(3,4)" == [Instruction.mul 1 2, Instruction.mul 3 4]
#guard findInstructions "do()" == [Instruction.do]
#guard findInstructions "don't()" == [Instruction.dont]

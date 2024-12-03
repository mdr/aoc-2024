import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Regex
open Regex (Match Captures)

inductive Instruction where
  | mul (n m : Nat) : Instruction
  -- | do : Instruction
  -- | dont: Instruction
deriving instance BEq, Hashable, Repr for Instruction

def mulRegex: Regex := regex% r"mul\((\d+),(\d+)\)"

def matchToNat (m : Match) : Option Nat := m.toNat?

def optMatchToNat (m : Option Match) : Option Nat := m >>= matchToNat

def matchesToNatPair (ms : Array (Option Match)): Option Instruction :=
  match ms.toList with
  | [some m1, some m2] => Instruction.mul <$> matchToNat m1 <*> matchToNat m2
  | _ => none

def capturesToNatPair (captures: Captures) : Option Instruction :=
  matchesToNatPair captures.groups

def findMatches (s : String) : List Instruction :=
  Regex.all_captures s mulRegex |>.toList |>.bind (fun captures => captures |> capturesToNatPair |> Option.toList)

#guard findMatches "mul(1,2) mul(3,4)" == [Instruction.mul 1 2, Instruction.mul 3 4]

def solvePart1 (s : String) : Int := findMatches s |>.sumBy (fun
  | Instruction.mul n m => n * m
)

#guard solvePart1 exampleInput = 161

private def solvePart2 (things : List Int) : Int := sorry

def instrRegex := regex% r"mul\((\d+),(\d+)\)|do()\)"
#eval Regex.all_captures "mul(1,2) do()" instrRegex

def parseAndSolvePart2 (s : String): Except String Int := parseThings s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok 4

import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Regex
open Regex (Match Captures)

def mulRegex := regex% r"mul\((\d+),(\d+)\)"

def matchToNat (m : Match) : Option Nat := m.toNat?

def optMatchToNat (m : Option Match) : Option Nat := m >>= matchToNat

def matchesToNatPair (ms : Array (Option Match)): Option (Nat × Nat) :=
  match ms.toList with
  | [some m1, some m2] => Prod.mk <$> matchToNat m1 <*> matchToNat m2
  | _ => none

def capturesToNatPair (captures: Captures) : Option (Nat × Nat) :=
  matchesToNatPair captures.groups

def findMatches (s : String) : List (Nat × Nat) :=
  Regex.all_captures s mulRegex |>.toList |>.bind (fun captures => captures |> capturesToNatPair |> Option.toList)

#guard findMatches "mul(1,2) mul(3,4)" = [(1, 2), (3, 4)]

def solvePart1 (s : String) : Int := findMatches s |>.sumBy (fun (n1, n2) => n1 * n2)

#guard solvePart1 exampleInput = 161

private def solvePart2 (things : List Int) : Int := sorry

def parseAndSolvePart2 (s : String): Except String Int := parseThings s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok 4

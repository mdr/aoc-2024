import Aoc2024.Day02.Parser
import Aoc2024.Utils
import Batteries
import Aoc2024.Day02.Examples
open Std (HashSet)
namespace Aoc2024.Day02

private def differences (xs: List Int) : List Int :=
  xs.zip xs.tail |>.map (λ (a, b) => b - a)

#guard differences [] == []
#guard differences [1] == []
#guard differences [1, 2, 3, 2, 5] == [1, 1, -1, 3]

private def increasingSlowly: HashSet Int := HashSet.ofList [1, 2, 3]
private def decreasingSlowly: HashSet Int := HashSet.ofList [-1, -2, -3]

private def isSubsetOf [Hashable α] [BEq α] (xs: HashSet α) (ys: HashSet α) : Bool :=
  xs.all ys.contains

private def isSafe (report : Report) : Bool :=
  let diffs := differences report |> HashSet.ofList
  isSubsetOf diffs increasingSlowly || isSubsetOf diffs decreasingSlowly

#guard isSafe [7, 6, 4, 2, 1] == true
#guard isSafe [1, 2, 7, 8, 9] == false
#guard isSafe [9, 7, 6, 2, 1] == false
#guard isSafe [1, 3, 2, 4, 5] == false
#guard isSafe [8, 6, 4, 4, 1] == false
#guard isSafe [1, 3, 6, 7, 9] == true

private def solvePart1 (reports : List Report) : Int := reports.countP isSafe

def parseAndSolvePart1 (s : String): Except String Int := parseReports s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 2

private def removeNth (n: Nat) (xs: List α) : List α :=
  xs.take n ++ xs.drop (n + 1)

#guard removeNth 0 [1, 2, 3] == [2, 3]
#guard removeNth 1 [1, 2, 3] == [1, 3]
#guard removeNth 2 [1, 2, 3] == [1, 2]
#guard removeNth 3 [1, 2, 3] == [1, 2, 3]

private def isSafeWithTolerance (report : Report) : Bool :=
  List.range report.length |>.map (λ n => removeNth n report) |>.any isSafe

private def solvePart2 (reports : List Report) : Int := reports.countP isSafeWithTolerance

def parseAndSolvePart2 (s : String): Except String Int := parseReports s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 4

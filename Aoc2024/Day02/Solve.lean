import Aoc2024.Day02.Parser
import Aoc2024.Utils
import Batteries
import Aoc2024.Day02.Examples
open Std (HashSet)

private def increasingSlowly: HashSet Int := [1, 2, 3].toSet
private def decreasingSlowly: HashSet Int := [-1, -2, -3].toSet

private def isSafe (report : Report) : Bool :=
  let diffs := report.differences.toSet
  diffs.isSubsetOf increasingSlowly || diffs.isSubsetOf decreasingSlowly

#guard isSafe [7, 6, 4, 2, 1] == true
#guard isSafe [1, 2, 7, 8, 9] == false
#guard isSafe [9, 7, 6, 2, 1] == false
#guard isSafe [1, 3, 2, 4, 5] == false
#guard isSafe [8, 6, 4, 4, 1] == false
#guard isSafe [1, 3, 6, 7, 9] == true

private def solvePart1 (reports : List Report) : Int := reports.countP isSafe

def parseAndSolvePart1 (s : String): Except String Int := parseReports s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 2

private def isSafeWithTolerance (report : Report) : Bool :=
  List.range report.length |>.map report.removeNth |>.any isSafe

private def solvePart2 (reports : List Report) : Int := reports.countP isSafeWithTolerance

def parseAndSolvePart2 (s : String): Except String Int := parseReports s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 4

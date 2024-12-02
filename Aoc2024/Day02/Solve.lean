import Aoc2024.Day02.Parser
import Aoc2024.Utils
-- import Aoc2024.Day02.Examples
namespace Aoc2024.Day02

private def differences (xs: List Int) : List Int :=
  xs.zip xs.tail |>.map (λ (a, b) => b - a)

#guard differences [] == []
#guard differences [1] == []
#guard differences [1, 2, 3, 2, 5] == [1, 1, -1, 3]

private def isSafe (report : Report) : Bool :=
  -- let diffs := differences report
  -- diffs.all (λ d => d == 1 || d == 3)
  panic! "TODO"

private def solvePart1 (reports : List Report) : Int := panic! "TODO"

def parseAndSolvePart1 (s : String): Except String Int := parseReports s |>.map solvePart1

-- #guard parseAndSolvePart1 exampleInput == Except.ok 11

-- private def solvePart2 (pairs : List (Int × Int)) : Int :=
--   let (firsts, seconds) := pairs.unzip
--   let similarityScore (n : Int) : Int := seconds.count n * n
--   firsts.sumBy similarityScore

-- def parseAndSolvePart2 (s : String): Except String Int := parseLines s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok 31

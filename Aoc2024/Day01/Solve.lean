import Aoc2024.Utils
import Aoc2024.Day01.Examples
import Aoc2024.Day01.Parser

private def solvePart1 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let distance (a b : Int) : Int := (a - b).natAbs
  (firsts.mergeSort.zipWith distance seconds.mergeSort).sum

def parseAndSolvePart1 (s : String): Except String Int := parseLines s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 11

private def solvePart2 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let similarityScore (n : Int) : Int := seconds.count n * n
  firsts.sumBy similarityScore

def parseAndSolvePart2 (s : String): Except String Int := parseLines s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 31

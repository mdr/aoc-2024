import Aoc2024.Day01.Parser
import Aoc2024.Utils
namespace Aoc2024.Day01

private def exampleInput :=
"3   4
4   3
2   5
1   3
3   9
3   3"

private def solvePart1 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let distance (a b : Int) : Int := (a - b).natAbs
  (firsts.mergeSort.zipWith distance seconds.mergeSort) |> sumList

def parseAndSolvePart1 : String -> Except String Int := .map solvePart1 ∘ parseLines

#guard (parseAndSolvePart1 exampleInput == Except.ok 11)

private def solvePart2 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let similarityScore (n : Int) : Int := seconds.count n * n
  firsts |> sumListBy similarityScore

def parseAndSolvePart2 : String -> Except String Int := .map solvePart2 ∘ parseLines

#guard (parseAndSolvePart2 exampleInput == Except.ok 31)

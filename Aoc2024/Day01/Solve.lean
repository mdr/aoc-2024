import Aoc2024.Day01.Parser
namespace Aoc2024.Day01

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let distance (a b : Int) : Int := (a - b).natAbs
  (firsts.mergeSort.zipWith distance seconds.mergeSort) |> sumList

def parseAndSolvePart1 : String -> Except String Int := .map solvePart1 ∘ parseLines

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let (firsts, seconds) := pairs.unzip
  let similarityScore (n : Int) : Int := seconds.count n * n
  firsts |> sumListBy similarityScore

def parseAndSolvePart2 : String -> Except String Int := .map solvePart2 ∘ parseLines

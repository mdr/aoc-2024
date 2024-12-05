import Aoc2024.Utils
import Aoc2024.Day05.Examples
import Aoc2024.Day05.Parser
open Std (HashSet)

-- Part 1

private def middleElement {α} (l : List α) : Option α := l.get? (l.length / 2)

private def middlePageNumber (update : Update) : Int := middleElement update.pages |>.getD 0

private def isCorrectlyOrdered (rules : List OrderingRule) (update : Update) : Bool :=
  let allowedPairs := rules.map (·.toPair) |>.toSet
  update.pages.slidingPairs.toSet.isSubsetOf allowedPairs

private def solvePart1 (input : PuzzleInput) : Int :=
  input.updates.filter (isCorrectlyOrdered input.rules)
    |>.sumBy middlePageNumber

def parseAndSolvePart1 (s : String): Except String Int := parseInput s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 143

-- Part 2

private def orderCorrectly (rules : List OrderingRule) (update : Update) : Update :=
  let allowedPairs := rules.map (·.toPair) |>.toSet
  let reorderedPages := update.pages.mergeSort (λ p1 p2 => p1 == p2 || allowedPairs.contains (p1, p2))
  { pages := reorderedPages }

private def solvePart2 (input : PuzzleInput) : Int :=
  input.updates.filterNot (isCorrectlyOrdered input.rules)
    |>.map (orderCorrectly input.rules)
    |>.sumBy middlePageNumber

def parseAndSolvePart2 (s : String): Except String Int := parseInput s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 123

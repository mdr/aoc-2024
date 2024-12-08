import Aoc2024.Utils
import Aoc2024.Day08.Examples
import Aoc2024.Day08.Parser
import Aoc2024.Day08.Types
import Std
open Std (HashMap)

-- Part 1

private def getAntennaeGroups (input : PuzzleInput) : List (List Point) :=
  input.decoratedChars |>.groupByAndTransformValues (·.char) (·.point) |>.values

private def findAntinodes (antennaGroup : List Point) : List Point := do
  let antenna1 <- antennaGroup
  let antenna2 <- antennaGroup
  if antenna1 != antenna2 then
    return antenna2.add (antenna1.vectorTo antenna2)
  []

private def solvePart1 (input : PuzzleInput) : Int :=
  getAntennaeGroups input |>.flatMap findAntinodes |>.filter (input.bounds.contains) |>.toSet.size

def parseAndSolvePart1 (s : String): Int := parseInput s |> solvePart1

#guard parseAndSolvePart1 exampleInput == 14

-- Part 2

private def combinationPairs [BEq α] (xs : List α) : List (α × α) := do
  let [x1, x2] <- xs.combinations 2 | []
  return (x1, x2)

private def Point.isColinearWith (point : Point) (pair : (Point × Point)) : Bool :=
  let (p1, p2) := pair
  let v1 := p1.vectorTo p2
  let v2 := p1.vectorTo point
  v1.x * v2.y == v1.y * v2.x

private def solvePart2 (input : PuzzleInput) : Int :=
  let antennaePairs := getAntennaeGroups input |>.flatMap combinationPairs
  let isAntinode (point : Point) : Bool := antennaePairs.any point.isColinearWith -- exploiting special property of the input
  input.bounds.allPoints.countP isAntinode

def parseAndSolvePart2 (s : String): Int := parseInput s |> solvePart2

#guard parseAndSolvePart2 exampleInput == 34

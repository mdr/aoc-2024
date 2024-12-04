import Aoc2024.Utils
import Aoc2024.Day04.Examples
import Aoc2024.Day04.Parser
import Batteries

-- Part 1

private def countXmasOccurrencesInRow : List Char -> Int
  | [] => 0
  | 'X' :: 'M' :: 'A' :: 'S' :: rest => 1 + countXmasOccurrencesInRow ('S' :: rest) -- continue scan from S as it could start a SAMX
  | 'S' :: 'A' :: 'M' :: 'X' :: rest => 1 + countXmasOccurrencesInRow ('X' :: rest) -- continue scan from X as it could start a XMAS
  | _ :: rest => countXmasOccurrencesInRow rest

#guard countXmasOccurrencesInRow "ABXMASXMASXMA".toList == 2

private def countXmasOccurrencesInRows (grid : Grid): Int := grid.sumBy countXmasOccurrencesInRow

private def gridToString (g : Grid) : String := g.map List.asString |> String.intercalate "\n"

#guard gridToString [['A', 'B', 'C'], ['D', 'E', 'F']] = "ABC\nDEF"

private def flipHorizontal (grid : Grid) : Grid := grid.map (·.reverse)

private def allGridTransformations : List (Grid -> Grid) := [
  id,
  List.transpose,
  List.diagonals,
  List.diagonals ∘ flipHorizontal,
]

private def transformGridInAllWays (grid : Grid) : List Grid := allGridTransformations.map (· grid)

def solvePart1 (s : String): Int := parseGrid s |> transformGridInAllWays |>.sumBy countXmasOccurrencesInRows

#guard solvePart1 exampleInput1 = 4
#guard solvePart1 exampleInput2 = 18

-- Part 2

private def slidingGrids (grid : Grid): List Grid :=
  grid.slidingWindows 3 |>.bind (λ rowBundle => rowBundle.map (List.slidingWindows 3) |>.transpose)

#guard slidingGrids [['A', 'B', 'C', 'D', 'E'], ['F', 'G', 'H', 'I', 'J'], ['K', 'L', 'M', 'N', 'O']] ==
[
  [
    ['A', 'B', 'C'],
    ['F', 'G', 'H'],
    ['K', 'L', 'M']
  ], [
    ['B', 'C', 'D'],
    ['G', 'H', 'I'],
    ['L', 'M', 'N']
  ], [
    ['C', 'D', 'E'],
    ['H', 'I', 'J'],
    ['M', 'N', 'O']
  ]
]

private def is2Mas: Grid -> Bool
  | [r1, r2, r3] =>
    let match1 := match r1 with | ['M', _, 'S'] => true | _ => false
    let match2 := match r2 with | [_, 'A', _] => true | _ => false
    let match3 := match r3 with | ['M', _, 'S'] => true | _ => false
    match1 && match2 && match3
  | _ => false

private def rotate90 (grid : Grid) : Grid := grid.transpose.map (·.reverse)

private def allRotations : List (Grid -> Grid) :=
  [id, rotate90, iterate 2 rotate90, iterate 3 rotate90]

private def rotateAll (grid : Grid) : List Grid := allRotations.map (· grid)

def solvePart2 (s : String) : Int :=
  parseGrid s |> slidingGrids |>.bind rotateAll |>.countP is2Mas

#guard solvePart2 exampleInput2 = 9

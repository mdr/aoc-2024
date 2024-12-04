import Aoc2024.Utils
import Aoc2024.Day04.Examples
import Aoc2024.Day04.Parser
import Batteries

private def countXmasOccurrencesInRow : List Char -> Int
  | [] => 0
  | 'X' :: 'M' :: 'A' :: 'S' :: rest => 1 + countXmasOccurrencesInRow rest
  | _ :: rest => countXmasOccurrencesInRow rest

#guard countXmasOccurrencesInRow "ABXMASXMASXMA".toList == 2

private def countXmasOccurrencesInRows (grid : Grid): Int :=
  grid.sumBy countXmasOccurrencesInRow

private def gridToString (g : Grid) : String := g.map List.asString |> String.intercalate "\n"

#guard gridToString [['A', 'B', 'C'], ['D', 'E', 'F']] = "ABC\nDEF"

private def flipHorizontal (grid : Grid) : Grid := grid.map (·.reverse)

private def diagonals (grid : List (List α)) : List (List α) :=
  let rec go (b : List (List α)) (es : List (List α)) : List (List α) :=
    let diagonal := b.filterMap List.head?
    let ts := b.filterMap List.tail?
    diagonal :: match es with
      | [] => ts.transpose
      | e :: es' => go (e :: ts) es'
  (go [] grid).tail!

#guard diagonals [
  ["A", "B", "C", "D"],
  ["E", "F", "G", "H"],
  ["I", "J", "K", "L"],
] == [
  ["A"],
  ["E", "B"],
  ["I", "F", "C"],
  ["J", "G", "D"],
  ["K", "H"],
  ["L"],
]

private def allGridTransformations : List (Grid -> Grid) := [
  id,
  flipHorizontal,
  List.transpose,
  flipHorizontal ∘ List.transpose,
  diagonals,
  diagonals ∘ List.transpose,
  diagonals ∘ flipHorizontal,
  diagonals ∘ List.transpose ∘ flipHorizontal,
]

-- def g := parseGrid exampleInput1
-- #eval g |> id
-- #eval g |> flipHorizontal
-- #eval g |> List.transpose
-- #eval g |> List.transpose |> flipHorizontal
-- #eval g |> diagonals
-- #eval g |> List.transpose |> diagonals
-- #eval g |> flipHorizontal |> diagonals
-- #eval g |> flipHorizontal |> List.transpose |> diagonals

def solvePart1 (s : String): Int :=
  let grid := parseGrid s
  allGridTransformations.sumBy (λ transformation => transformation grid |> countXmasOccurrencesInRows)

#guard solvePart1 exampleInput1 = 4
#guard solvePart1 exampleInput2 = 18

def solvePart2 (things : s) : Int := sorry

-- def parseAndSolvePart2 (s : String): Except String Int := parseGrid s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok 4

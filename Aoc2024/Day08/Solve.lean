import Aoc2024.Utils
import Aoc2024.Day08.Examples
import Aoc2024.Day08.Parser
import Aoc2024.Day08.Types

-- Part 1

private def groupByChar (decoratedChars : List (Point × Char)) : List (Char × List Point) :=
  decoratedChars.groupBy (λ (_, c1) (_, c2) => c1 == c2) |>.map (λ l => (l.head!.2, l.map (·.1)))

#guard groupByChar [(⟨0, 0⟩, 'a'), (⟨1, 0⟩, 'a'), (⟨0, 1⟩, 'b'), (⟨1, 1⟩, 'b')] == [
  ('a', [⟨0, 0⟩, ⟨1, 0⟩]),
  ('b', [⟨0, 1⟩, ⟨1, 1⟩])
]

private def removeDots (decoratedChars : List (Point × Char)) : List (Point × Char) :=
  decoratedChars.filter (λ (_, c) => c != '.')

private def solvePart1 (input : PuzzleInput) : Int :=
  let groups := input.decoratedChars |> removeDots |> groupByChar
  sorry

def parseAndSolvePart1 (s : String): Int := parseInput s |> solvePart1

-- #guard parseAndSolvePart1 exampleInput == 14

-- Part 2

private def solvePart2 (input : PuzzleInput) : Int := sorry

def parseAndSolvePart2 (s : String): Int := parseInput s |> solvePart2

-- #guard parseAndSolvePart2 exampleInput == -1

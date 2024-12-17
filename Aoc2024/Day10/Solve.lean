import Aoc2024.Utils
import Aoc2024.Day10.Examples
import Aoc2024.Day10.Parser
import Aoc2024.Day10.Types
open Std (HashSet HashMap)

-- Part 1

private def Point.orthogonalNeighbours (p : Point): List Point :=
  let ⟨x, y⟩ := p
  [⟨x, y - 1⟩, ⟨x + 1, y⟩, ⟨x, y + 1⟩, ⟨x - 1, y⟩]

private def solvePart1 (input : PuzzleInput) : Int :=
  let heights := input.heights
  let trailheads := heights.invert.getD 0 []
  let getNeighbours (p : Point) (height : Height): List Point :=
    p.orthogonalNeighbours.filter (λ neighbour => heights.get? neighbour == some height)
  let score (trailhead : Point): Int :=
    let rec findReachablePeaks (p : Point) : Nat -> List Point
      | 0 => [p]
      | n + 1 =>
        let targetHeight := 9 - n
        let neighbours := getNeighbours p targetHeight
        neighbours.flatMap (findReachablePeaks · n)
    findReachablePeaks trailhead 9 |>.toSet.size
  trailheads.sumBy score

def parseAndSolvePart1 (s : String): Int := parseHeightMap s |> solvePart1

#guard parseAndSolvePart1 exampleInput == 36

-- Part 2

private def getNeighbours (heights : HashMap Point Height) (p : Point) (height : Height): List Point :=
    p.orthogonalNeighbours.filter (λ neighbour => heights.get? neighbour == some height)

private partial def countPaths (heights : HashMap Point Height) (p : Point) (currentHeight : Height): Int :=
  match currentHeight with
  | 9 => 1
  | _ =>
    let nextHeight := currentHeight + 1
    getNeighbours heights p nextHeight |>.sumBy (countPaths heights · nextHeight)

private def solvePart2 (input : PuzzleInput) : Int :=
  let heights := input.heights
  let trailheads := heights.invert.getD 0 []
  let score (trailhead : Point): Int := countPaths heights trailhead 0
  trailheads.sumBy score

def parseAndSolvePart2 (s : String): Int := parseHeightMap s |> solvePart2

#guard parseAndSolvePart2 exampleInput == 81

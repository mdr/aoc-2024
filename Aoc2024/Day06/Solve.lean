import Aoc2024.Utils
import Aoc2024.Day06.Examples
import Aoc2024.Day06.Parser

private def solvePart1 (input : PuzzleInput) : Int := sorry

def parseAndSolvePart1 (s : String): Except String Int := parseInput s |>.map solvePart1

-- #guard parseAndSolvePart1 exampleInput == Except.ok 41

private def solvePart2 (input : PuzzleInput) : Int := sorry

def parseAndSolvePart2 (s : String): Except String Int := parseInput s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok -1

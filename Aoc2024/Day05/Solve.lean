import Aoc2024.Utils
import Aoc2024.Day05.Examples
import Aoc2024.Day05.Parser

private def solvePart1 (things : List Int) : Int := sorry

def parseAndSolvePart1 (s : String): Except String Int := parseThings s |>.map solvePart1

-- #guard parseAndSolvePart1 exampleInput == Except.ok 2

private def solvePart2 (things : List Int) : Int := sorry

def parseAndSolvePart2 (s : String): Except String Int := parseThings s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok 4

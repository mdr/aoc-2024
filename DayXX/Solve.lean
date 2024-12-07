import Aoc2024.Utils
import Aoc2024.DayXX.Examples
import Aoc2024.DayXX.Parser
import Aoc2024.DayXX.Types

-- Part 1

private def solvePart1 (input : List Int) : Int := sorry

def parseAndSolvePart1 (s : String): Except String Int := parseInput s |>.map solvePart1

-- #guard parseAndSolvePart1 exampleInput == Except.ok -1

-- Part 2

private def solvePart2 (input : List Int) : Int := sorry

def parseAndSolvePart2 (s : String): Except String Int := parseInput s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok -1

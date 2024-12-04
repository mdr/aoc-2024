import Aoc2024.Day04.Examples
import Aoc2024.Day04.Types
import Aoc2024.Utils
import Std

def parseGrid (s : String) : Grid := s.lines.map (·.toList)

-- #guard parseReports exampleInput == Except.ok 42

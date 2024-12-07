import Aoc2024.Utils
import Aoc2024.Day07.Examples
import Aoc2024.Day07.Parser
import Aoc2024.Day07.Types

-- Part 1

private def evalNumbers (numbers : List Int) (operators : List Operator): Int :=
  numbers.zip (.add :: operators) |>.foldl (λ acc (n, op) => op.fn acc n) 0

#guard evalNumbers [11, 6, 16, 20] [.add, .multiply, .add] == 292

private def Equation.isTrue (equation : Equation) (operators : List Operator) : Bool :=
  evalNumbers equation.numbers operators == equation.testValue

private def Equation.operatorPositions (equation : Equation) : Nat := equation.numbers.length - 1

private def canEquationPossiblyBeTrue (operators : List Operator) (equation : Equation) : Bool :=
  replicateM equation.operatorPositions operators |>.any equation.isTrue

private def solveGeneric (operators : List Operator) (equations : List Equation) : Int :=
  equations.filter (canEquationPossiblyBeTrue operators) |>.sumBy (·.testValue)

private def solvePart1 : List Equation -> Int := solveGeneric [.add, .multiply]

def parseAndSolvePart1 (s : String): Except String Int := parseEquations s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 3749

-- Part 2

private def solvePart2 : List Equation -> Int := solveGeneric [.add, .multiply, .concatenation]

def parseAndSolvePart2 (s : String): Except String Int := parseEquations s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 11387

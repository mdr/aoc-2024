import Aoc2024.Utils
import Aoc2024.Day07.Examples
import Aoc2024.Day07.Parser
import Aoc2024.Day07.Types

-- Part 1

private def concatenate (a : Int) (b : Int) : Int := (reprStr a ++ reprStr b).toInt!

private def evalNumbers (numbers : List Int) (operators : List Operator): Int :=
  match numbers with
    | [] => 0
    | h :: t =>
      t.zip operators |>.foldl (fun acc (n, op) => match op with
        | .add => acc + n
        | .multiply => acc * n
        | .concatenation => concatenate acc n
      ) h

#guard evalNumbers [11, 6, 16, 20] [.add, .multiply, .add] == 292

private def replicateM (n : Nat) (options : List α) : List (List α) :=
  let rec loop (n : Nat) (acc : List (List α)) : List (List α) :=
    match n with
    | 0 => acc
    | n + 1 => loop n (acc.bind λ l => options.map (λ o => o :: l))
  loop n [[]]
#guard replicateM 0 [1, 2] == [[]]
#guard replicateM 3 [1, 2] == [[1, 1, 1], [2, 1, 1], [1, 2, 1], [2, 2, 1], [1, 1, 2], [2, 1, 2], [1, 2, 2], [2, 2, 2]]

private def canEquationPossiblyBeTrue (operators : List Operator) (equation : Equation) : Bool :=
  let operators := replicateM equation.numbers.length operators
  operators.any λ ops => evalNumbers equation.numbers ops == equation.testValue

private def solvePart1 (equations : List Equation) : Int :=
  equations.filter (canEquationPossiblyBeTrue [.add, .multiply]) |>.sumBy (·.testValue)

def parseAndSolvePart1 (s : String): Except String Int := parseEquations s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 3749

-- Part 2

private def solvePart2 (equations : List Equation) : Int :=
  equations.filter (canEquationPossiblyBeTrue [.add, .multiply, .concatenation]) |>.sumBy (·.testValue)

def parseAndSolvePart2 (s : String): Except String Int := parseEquations s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 11387

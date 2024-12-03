import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Aoc2024.Day03.Types

def Instruction.value : Instruction -> Int
  | Instruction.mul n m => n * m
  | Instruction.do => 1
  | Instruction.dont => 1

def solvePart1 (s : String) : Int := findInstructions s |>.sumBy (·.value)

#guard solvePart1 exampleInputPart1 == 161

private def filterInstructions (enabled : Bool) : List Instruction -> List Instruction
  | [] => []
  | List.cons instruction rest =>
    match instruction with
      | Instruction.mul m n =>
        if enabled then List.cons (Instruction.mul m n) (filterInstructions enabled rest)
        else filterInstructions enabled rest
      | Instruction.do => filterInstructions true rest
      | Instruction.dont => filterInstructions false rest

def solvePart2 (s : String) : Int := findInstructions s |> filterInstructions true |> List.sumBy (·.value)

#guard solvePart2 exampleInputPart2 == 48

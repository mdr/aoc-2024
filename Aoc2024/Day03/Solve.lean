import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Aoc2024.Day03.Types

def Instruction.value : Instruction -> Int
  | Instruction.mul n m => n * m
  | _ => 1

def solvePart1 (s : String) : Int := findInstructions s |>.sumBy (·.value)

#guard solvePart1 exampleInputPart1 == 161

private def findEnabledMulInstructions (enabled : Bool) : List Instruction -> List Instruction
  | [] => []
  | instruction :: rest =>
    match instruction with
      | Instruction.mul m n =>
        if enabled then
          Instruction.mul m n ::findEnabledMulInstructions enabled rest
        else
          findEnabledMulInstructions enabled rest
      | Instruction.do => findEnabledMulInstructions true rest
      | Instruction.dont => findEnabledMulInstructions false rest

def solvePart2 (s : String) : Int := findInstructions s |> findEnabledMulInstructions true |>.sumBy (·.value)

#guard solvePart2 exampleInputPart2 == 48

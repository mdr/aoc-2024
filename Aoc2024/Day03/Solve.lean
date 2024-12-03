import Aoc2024.Utils
import Aoc2024.Day03.Examples
import Aoc2024.Day03.Parser
import Aoc2024.Day03.Types

-- Part 1

private def Instruction.value : Instruction -> Int
  | Instruction.mul n m => n * m
  | _ => 1

def solvePart1 (s : String) : Int := findInstructions s |>.sumBy (·.value)

#guard solvePart1 exampleInputPart1 == 161

-- Part 2

private structure State where
  isEnabled: Bool
  enabledMuls: List Instruction

private def State.enable (s : State): State := { s with isEnabled := true }
private def State.disable (s : State): State := { s with isEnabled := false }
private def State.recordMul (instruction : Instruction) (s : State): State :=
  { s with enabledMuls := instruction :: s.enabledMuls }
private def State.initial: State := { isEnabled := true, enabledMuls := [] }

private def processInstruction : Instruction -> StateM State Unit
  | i@(Instruction.mul _ _) => do
      if (<- get).isEnabled then
        modify (State.recordMul i)
  | Instruction.do => modify State.enable
  | Instruction.dont => modify State.disable

private def getEnabledMuls (instructions : List Instruction) : List Instruction :=
  instructions.forM processInstruction |>.run State.initial |>.snd.enabledMuls

def solvePart2 (s : String) : Int := findInstructions s |> getEnabledMuls |>.sumBy (·.value)

#guard solvePart2 exampleInputPart2 == 48

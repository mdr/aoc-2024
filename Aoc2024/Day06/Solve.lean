import Aoc2024.Utils
import Aoc2024.Day06.Examples
import Aoc2024.Day06.Parser
import Std
open Std (HashSet)

-- Part 1

private structure PatrolState where
  position : Point
  direction : Direction := Direction.Up
  obstacles : HashSet Point
  bounds : Rectangle
  visited : HashSet Point
  hasLooped : Bool := false
deriving Repr

private def PatrolState.hasObstacle (s : PatrolState) (p : Point): Bool := s.obstacles.contains p

private def PatrolState.initial (input : PuzzleInput) : PatrolState :=
  { position := input.start, obstacles := input.obstacles, bounds := input.bounds, visited := singleton input.start }

private def PatrolState.positionAhead (s : PatrolState) : Point := s.position.step s.direction

private def PatrolState.turnRight (s : PatrolState) : PatrolState := { s with direction := s.direction.turnRight }

private def PatrolState.stepForward (s : PatrolState) : PatrolState :=
  let newPosition := s.position.step s.direction
  let hasLooped := s.visited.contains newPosition
  { s with position := newPosition, visited := s.visited.insert newPosition, hasLooped }

private def PatrolState.addObstacle (s : PatrolState) (p : Point) : PatrolState :=
  { s with obstacles := s.obstacles.insert p }

-- returns true iff the guard has left the area
private def stepGuard : StateM PatrolState Bool := do
  let state ← get
  let ahead := state.positionAhead
  if state.hasObstacle ahead then
    modify PatrolState.turnRight *> return false
  else if state.bounds.contains ahead then
    modify PatrolState.stepForward *> return false
  else
    return true

-- returns true iff simulation has ended early by running out of fuel
private def simulatePatrol : Nat -> StateM PatrolState Bool
  | 0 => return true
  | fuel + 1 => do
    let hasLeftArea <- stepGuard
    if (hasLeftArea) then
      return false
    else
      simulatePatrol fuel

private def MAX_FUEL := 100000

private def solvePart1 (input : PuzzleInput) : Option Int :=
  let initialState := PatrolState.initial input
  let (simulationEndedEarly, finalState) := (simulatePatrol MAX_FUEL).run initialState
  if simulationEndedEarly then none else some finalState.visited.size

def parseAndSolvePart1 (s : String): Except String Int := do
  solvePart1 (<- parseInput s) |>.getOrThrow "simulation ended early"

#guard parseAndSolvePart1 exampleInput == Except.ok 41

-- Part 2

private def doesPatrolLoop (initialState: PatrolState): Bool :=
  let (_, finalState) := (simulatePatrol MAX_FUEL).run initialState
  finalState.hasLooped

private def solvePart2 (input : PuzzleInput) : Int :=
  let initialState := PatrolState.initial input
  let (_, finalState) := (simulatePatrol MAX_FUEL).run initialState
  let candidateObstacles := finalState.visited.erase input.start
  candidateObstacles.toList.countP (doesPatrolLoop ∘ initialState.addObstacle)

def parseAndSolvePart2 (s : String): Except String Int := parseInput s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 6

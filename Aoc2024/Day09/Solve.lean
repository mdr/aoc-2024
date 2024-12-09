import Aoc2024.Utils
import Aoc2024.Day09.Examples
import Aoc2024.Day09.Parser
import Aoc2024.Day09.Types

-- Part 1

abbrev Blocks := Array (Option IdNum)

private def blocksToString (blocks : Blocks) : String :=
  let b := blocks.map fun
    | none => "."
    | some id => toString id
  b.foldl (· ++ ·) ""
#guard blocksToString #[none, none, some 1, none, some 2] == "..1.2"

inductive ExpandMode where
  | files : ExpandMode
  | freeSpace : ExpandMode

private def expandDiskMap (diskMap : List Nat) : Blocks :=
  let rec expand (nextId: IdNum) : ExpandMode -> List Nat -> List (Option IdNum)
    | _, [] => []
    | .files, d :: rest => List.replicate d (some nextId) ++ expand (nextId + 1) ExpandMode.freeSpace rest
    | .freeSpace, d :: rest => List.replicate d none ++ expand nextId ExpandMode.files rest
  expand 0 ExpandMode.files diskMap |>.toArray

#guard (parseDiskMap exampleInput |>.map expandDiskMap |>.map blocksToString) ==
  Except.ok "00...111...2...333.44.5555.6666.777.888899"

private def nextFreeBlockOnOrAfter (i : Nat) (blocks : Blocks) : Nat :=
  let rec loop (i : Nat) : Nat :=
    if i < blocks.size then
      match blocks.get! i with
      | none => i
      | some _ => loop (i + 1)
    else
      i
  loop i

#guard nextFreeBlockOnOrAfter 1 #[none, some 0, some 0, none, none, none, some 1] == 3

private def nextFileBlockOnOrBefore (i : Nat) (blocks : Blocks) : Nat :=
  let rec loop (i : Nat) : Nat :=
    if i > 0 then
      match blocks.get! i with
      | none => loop (i - 1)
      | some _ => i
    else
      i
  loop i

#guard nextFileBlockOnOrBefore 4 #[some 0, some 0, none, none, none, some 1] == 1
#guard nextFileBlockOnOrBefore 5 #[some 0, some 0, none, none, none, some 1] == 5

private def swapBlocks (blocks : Blocks) (index1 index2 : Nat) : Blocks :=
  let value1 := blocks.getD index1 none
  let value2 := blocks.getD index2 none
  blocks.setD index1 value2 |>.setD index2 value1

#guard swapBlocks #[some 0, some 1, none, none, none, some 2] 1 5 ==
  #[some 0, some 2, none, none, none, some 1]

private def checksum (blocks : Blocks) : Int :=
  blocks.toList.enum.sumBy fun
    | (_, none) => 0
    | (i, some id) => id * i

private def solvePart1 (diskMap : List Nat) : Int := Id.run do
  let mut blocks := expandDiskMap diskMap
  let mut nextFree := nextFreeBlockOnOrAfter 0 blocks
  let mut nextFile := blocks.size - 1 -- assumption on input: ends with file blocks
  while nextFree < nextFile do
    blocks := swapBlocks blocks nextFree nextFile
    nextFree := nextFreeBlockOnOrAfter (nextFree + 1) blocks
    nextFile := nextFileBlockOnOrBefore (nextFile - 1) blocks
  checksum blocks

def parseAndSolvePart1 (s : String): Except String Int := parseDiskMap s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 1928

-- Part 2

private def solvePart2 (diskMap : List Nat) : Int := sorry

def parseAndSolvePart2 (s : String): Except String Int := parseDiskMap s |>.map solvePart2

-- #guard parseAndSolvePart2 exampleInput == Except.ok -1

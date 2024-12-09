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
      match blocks.getD i none with
      | none => i
      | some _ => loop (i + 1)
    else
      i
  loop i

#guard nextFreeBlockOnOrAfter 1 #[none, some 0, some 0, none, none, none, some 1] == 3

private def nextFileBlockOnOrBefore (i : Nat) (blocks : Blocks) : Nat :=
  let rec loop (i : Nat) : Nat :=
    if i > 0 then
      match blocks.getD i none with
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
  blocks.toList.enum.sumBy λ
    | (_, none) => 0
    | (i, some id) => id * i

private def solvePart1 (diskMap : List Nat) : Int := Id.run do
  let mut blocks := expandDiskMap diskMap
  let mut nextFree := nextFreeBlockOnOrAfter 0 blocks
  let mut nextFile := nextFileBlockOnOrBefore (blocks.size - 1) blocks
  while nextFree < nextFile do
    blocks := swapBlocks blocks nextFree nextFile
    nextFree := nextFreeBlockOnOrAfter (nextFree + 1) blocks
    nextFile := nextFileBlockOnOrBefore (nextFile - 1) blocks
  checksum blocks

def parseAndSolvePart1 (s : String): Except String Int := parseDiskMap s |>.map solvePart1

#guard parseAndSolvePart1 exampleInput == Except.ok 1928

-- Part 2

private def swapBlockRange (blocks : Blocks) (index1 index2 length : Nat) : Blocks := Id.run do
  let mut blocks := blocks
  for i in [0:length] do
    blocks := swapBlocks blocks (index1 + i) (index2 + i)
  blocks

#guard swapBlockRange #[some 0, none, none, none, some 9, some 9, none] (index1 := 1) (index2 := 4) (length := 2) ==
  #[some 0, some 9, some 9, none, none, none, none]

private def getHighestId (blocks : Blocks) : IdNum :=
  blocks.foldl (fun max block => match block with
    | none => max
    | some id => Nat.max max id) 0

structure Range where
  index: Nat
  length: Nat
deriving Repr, BEq

private def locateFile (blocks : Blocks) (id : IdNum) : Range :=
  let index := blocks.findIdx? (λ block => block == some id) |>.getD 0
  let length: Nat := Id.run do
    let mut length := 1
    while blocks.getD (index + length) none == some id do
      length := length + 1
    length
  { index, length }

#guard locateFile #[none, some 0, some 0, none, none, none, some 1] 0 == { index := 1, length := 2 }

private def leftmostFreeSpace (blocks : Blocks) (length : Nat): Option Nat :=
  let rec loop (n : Nat) : Option Nat :=
    match n with
    | 0 => none
    | n' + 1 =>
      let i := blocks.size - n
      if blocks.toSubarray i (i + length) |>.all (· == none) then
        some i
      else
        loop n'
  loop blocks.size

#guard leftmostFreeSpace #[some 0, none, none, some 1, none, none, none, some 2] 2 == some 1
#guard leftmostFreeSpace #[some 0, none, none, some 1, none, none, none, some 2] 3 == some 4

private def solvePart2 (diskMap : List Nat) : Int := Id.run do
  let mut blocks := expandDiskMap diskMap
  let mut currentId := getHighestId blocks
  while true do
    -- dbg_trace blocksToString blocks
    let range := locateFile blocks currentId
    let maybeSpace := leftmostFreeSpace blocks range.length
    match maybeSpace with
    | none => blocks := blocks
    | some spaceIndex =>
      if spaceIndex < range.index then
        blocks := swapBlockRange blocks spaceIndex range.index range.length
    if currentId == 0 then
      break
    currentId := currentId - 1
  checksum blocks

def parseAndSolvePart2 (s : String): Except String Int := parseDiskMap s |>.map solvePart2

#guard parseAndSolvePart2 exampleInput == Except.ok 2858

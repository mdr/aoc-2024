namespace Aoc2024.Day01

def distance (a b : Int) : Int :=
  (a - b).natAbs

def sumList (nums : List Int) : Int :=
  nums.foldl (fun acc x => acc + x) 0

def solvePart1 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1) |>.mergeSort
  let seconds := pairs.map (·.2) |>.mergeSort
  let pairs := firsts.zipWith distance seconds
  sumList pairs

def solvePart2 (pairs : List (Int × Int)) : Int :=
  let firsts := pairs.map (·.1)
  let seconds := pairs.map (·.2)
  let countOccurrencesInSeconds (n : Int) : Int :=
    seconds.filter (· == n) |>.length
  let similarityScore (n : Int) : Int :=
    countOccurrencesInSeconds n * n
  firsts.map similarityScore |> sumList

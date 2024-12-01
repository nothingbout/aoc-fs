module AOC2018.Day01 
open Utils

let rec findRepeating (curFreq: int) (seenFreqs: Set<int>) (remainingChanges: List<int>) (allChanges: List<int>) : int  = 
    if seenFreqs.Contains curFreq then curFreq
    else match remainingChanges with
            | [] -> findRepeating curFreq seenFreqs allChanges allChanges
            | change :: remaining -> findRepeating (curFreq + change) (seenFreqs.Add curFreq) remaining allChanges

let solveP1 (inputLines: string list) = 
    inputLines |> List.map int |> List.fold (+) 0 |> Answer.int

let solveP2 (inputLines: string list) = 
    inputLines |> List.map int |> fun changes -> findRepeating 0 Set.empty changes changes |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 445)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 219)
    ]


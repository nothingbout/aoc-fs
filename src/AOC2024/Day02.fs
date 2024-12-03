module AOC2024.Day02
open Utils
open Utils.Globals

let parseReport line = 
    line |> String.splitByString " " |> List.map int

let isSafe report = 
    report |> Seq.pairwise |> Seq.forall (fun (a, b) -> a < b && b - a <= 3) || 
    report |> Seq.pairwise |> Seq.forall (fun (a, b) -> a > b && a - b <= 3)

let isSafeWithOneException report = 
    isSafe report ||
    seq { 0..List.length report - 1 } 
    |> Seq.exists (fun i -> report |> List.removeAt i |> isSafe)

let solveP1 (inputLines: string list) = 
    let reports = inputLines |> List.map parseReport
    reports |> List.filter isSafe |> List.length |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let reports = inputLines |> List.map parseReport
    reports |> List.filter isSafeWithOneException |> List.length |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 2)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 242)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 4)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 311)
    ]

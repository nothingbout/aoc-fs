module AOC2018.DayXX
open Utils
open Utils.Globals

[<Struct>]
type Entry = { Str : string }

let parseEntry line = 
    line |> ScanSeq.ofString |> Scan.scan {
        let! str = Scan.all
        return { Str = str }
    } |> Scan.finish

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> List.map parseEntry
    Answer.int 0
    
let solveP2 (inputLines: string list) = 
    Answer.int 0

let getPuzzles() = 
    "aoc2018/dayXX", [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 0)
        // Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 0)
        // Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 0)
        // Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 0)
    ]

module AOC20XX.DayXX
open Utils

[<Struct>]
type Entry = { Str : string }

let parseEntry line = 
    line |> Substr.ofStr |> Scan.scan {
        let! str = Scan.takeAll
        return { Str = str |> toString }
    } |> Scan.finish

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> List.map parseEntry
    Answer.int 0
    
let solveP2 (inputLines: string list) = 
    Answer.int 0

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 0)
        // Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 0)
        // Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 0)
        // Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 0)
    ]

module AOC2024.Day25
open Utils

type Entry = 
    | Lock of int list
    | Key of int list

let countChars (char : char) (line : char list) = 
    line |> List.takeWhile (fun c -> c = char) |> List.length

let parseEntry (lines : string list) = 
    let lines = lines |> List.take 7 |> List.map (List.ofSeq) |> List.transpose
    if lines |> List.head |> List.head = '#' then
        lines |> List.map (countChars '#') |> List.map (fun c -> c - 1) |> Lock
    else
        lines |> List.map (countChars '.') |> List.map (fun c -> 6 - c) |> Key

let parseInput lines = 
    lines |> List.chunkBySize 8 |> List.map parseEntry

let fits a b = 
    match a, b with
    | Lock lock, Key key | Key key, Lock lock -> List.zip lock key |> List.forall (fun (l, k) -> l + k <= 5)
    | _ -> false

let solveP1 (inputLines: string list) = 
    let entries = parseInput inputLines
    let locks = entries |> List.filter (fun e -> e.IsLock)
    let keys = entries |> List.filter (fun e -> e.IsKey)

    let mutable count = 0
    for lock in locks do
        for key in keys do
            if fits lock key then 
                count <- count + 1

    Answer.int count

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 3)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 3619)
    ]

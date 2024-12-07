module AOC2024.Day07
open Utils
open Utils.Globals
open System.Numerics

[<Struct>]
type Entry = { Value : int64; Nums : int64 list }

let parseEntry line = 
    line |> Substr.ofStr |> Scan.scan {
        let! value = Scan.takeDigits
        do! Scan.skipString ": "
        let! rest = Scan.takeAll
        return { Value = int64 value; Nums = String.extractDigitGroups rest |> List.map int64 }
    } |> Scan.finish

let rec isTrueRec ops target cur xs = 
    if cur < 0L || cur > target then false
    else
    match xs with 
    | [] -> cur = target
    | x :: xs -> ops |> Seq.exists (fun op -> isTrueRec ops target (op cur x) xs)

let isTrue ops entry = 
    match entry.Nums with
    | x :: xs ->  isTrueRec ops entry.Value x xs
    | _ -> failwith "unexpected"

let solve ops inputLines = 
    let entries = inputLines |> List.map parseEntry
    entries |> List.filter (isTrue ops) |> List.sumBy (fun entry -> entry.Value)

let solveP1 (inputLines: string list) = 
    inputLines |> solve [( + ); ( * )] |> Answer.int64

let solveP2 (inputLines: string list) = 
    inputLines |> solve [( + ); ( * ); concatInt64s] |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 3749L)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 2654749936343L)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int64 11387L)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 124060392153684L)
    ]

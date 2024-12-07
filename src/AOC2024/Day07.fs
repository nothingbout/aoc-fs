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

let rec backsearch unops ys z = 
    match ys with 
    | [y] -> z = y
    | y :: xs -> unops |> Seq.exists (fun unop -> 
        match unop y z with
        | Some x -> backsearch unops xs x
        | None -> false
        )
    | [] -> failwith "unexpected"

let isTrue unops entry = 
    backsearch unops (List.rev entry.Nums) entry.Value

let unadd y z = 
    let x = z - y in if x < 0L then None else Some x

let unmul y z = 
    if z % y <> 0L then None else Some (z / y)

let uncat y z =
    let pow = pown 10L (countDigitsInt64 y) in if z % pow <> y then None else Some (z / pow)

let solve unops inputLines = 
    let entries = inputLines |> List.map parseEntry
    entries |> List.filter (isTrue unops) |> List.sumBy (fun entry -> entry.Value)

let solveP1 (inputLines: string list) = 
    inputLines |> solve [unadd; unmul] |> Answer.int64

let solveP2 (inputLines: string list) = 
    inputLines |> solve [unadd; unmul; uncat] |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 3749L)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 2654749936343L)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int64 11387L)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 124060392153684L)
    ]

module AOC2024.Day13
open Utils
open Utils.Globals

[<Struct>]
type Entry = { A : Vec2<int64>; B : Vec2<int64>; Prize : Vec2<int64> }

let parseVec line = 
    line |> String.extractInts |> List.map int64 |> fun ints ->
        match ints with
        | [a; b] -> Vec2.make a b
        | _ -> failwith "unexpected"

let rec parseEntries entries lines = 
    match lines with
    | l1 :: l2 :: l3 :: lines ->
        let entry = { A = parseVec l1; B = parseVec l2; Prize = parseVec l3 }
        match lines with
        | _ :: _ -> List.skip 1 lines |> parseEntries (entry :: entries) 
        | _ -> List.rev (entry :: entries)
    | _ -> failwith "unexpected"

let tryWin entry a =
    let b = (entry.Prize.X - entry.A.X * a) / entry.B.X
    if entry.A * a + entry.B * b = entry.Prize then 
        Some (a * 3L + b)
    else
        None

let slope v = 
    v.Y * 10000L / v.X

let tryFindWin entry = 
    if slope entry.A = slope entry.B then failwith "not implemented"

    let (<=?) = if slope entry.A > slope entry.B then (<=) else (>=)
    match (0L, entry.Prize.X / entry.A.X) ||> BinarySearch.lastLessOrEqual (fun a ->
        let b = (entry.Prize.X - entry.A.X * a) / entry.B.X
        let pos = entry.A * a + entry.B * b
        let delta = entry.Prize - pos
        if delta.X = 0 then 
            pos.Y <=? entry.Prize.Y 
        else 
            slope entry.B <=? slope delta
    ) with
    | None -> None
    | Some a -> tryWin entry a

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> parseEntries []
    entries |> List.choose tryFindWin |> List.sum |> Answer.int64
    
let solveP2 (inputLines: string list) = 
    let incr = 10_000_000_000_000L
    let entries = 
        inputLines |> parseEntries []
        |> List.map (fun entry -> {entry with Prize = entry.Prize + Vec2.make incr incr})
    entries |> List.choose tryFindWin |> List.sum |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 480)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 29522)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 101214869433312L)
    ]

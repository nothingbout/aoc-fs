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

let calcB entry a = 
    (entry.Prize.X - entry.A.X * a) / entry.B.X

let tryWin aStart entry = 
    seq {
        for a = aStart to aStart + 100L do
            let b = calcB entry a
            if entry.A * a + entry.B * b = entry.Prize then 
                yield a * 3L + b
    } 
    |> Seq.tryHead

let tryWinBig entry = 
    let aSteeper = entry.A.Y * 10000L / entry.A.X > entry.B.Y * 10000L / entry.B.X

    match (0L, entry.Prize.X / entry.A.X) ||> BinarySearch.lastLessOrEqual (fun a ->
        let y = entry.A.Y * a + entry.B.Y * (calcB entry a)
        if aSteeper then y <= entry.Prize.Y else y >= entry.Prize.Y
    ) with
    | None -> None
    | Some a -> tryWin (max 0 (a - 50L)) entry

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> parseEntries []
    entries |> List.choose (tryWin 0) |> List.sum |> Answer.int64
    
let solveP2 (inputLines: string list) = 
    let incr = 10_000_000_000_000L
    let entries = 
        inputLines |> parseEntries []
        |> List.map (fun entry -> {entry with Prize = entry.Prize + Vec2.make incr incr})
    entries |> List.choose tryWinBig |> List.sum |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 480)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 29522)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 101214869433312L)
    ]

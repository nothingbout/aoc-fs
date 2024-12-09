module AOC2024.Day09
open Utils
open Utils.Globals

let parseSizes lines = 
    lines |> String.concat "" |> Seq.map (fun c -> c - '0' |> int) |> Array.ofSeq

let solveP1 (inputLines: string list) = 
    let sizes = inputLines |> parseSizes
    let result = Array.init (Array.length sizes * 10) (fun _ -> -1)
    let mutable idx = 0
    for i, size in sizes |> Array.indexed do
        if i % 2 = 0 then
            for j = 0 to size - 1 do
                result[idx + j] <- i / 2
        idx <- idx + size

    let mutable start = 0
    let mutable finish = idx - 1
    while finish > start do
        while finish > start && result[finish] < 0 do
            finish <- finish - 1
        while result[start] >= 0 do
            start <- start + 1
        if finish > start then
           result[start] <- result[finish]
           result[finish] <- -1

    result |> Array.mapi (fun idx id -> if id >= 0 then int64 id * int64 idx else 0L)
    |> Array.sum |> Answer.int64

type File = { ID : int; Idx : int; Size : int }

let solveP2 (inputLines: string list) = 
    let sizes = inputLines |> parseSizes

    let mutable idx = 0
    let mutable files = sizes |> Array.chunkBySize 2 |> Array.mapi (fun id entries -> 
        let fileSize, freeSpace = 
            match entries with 
            | [|a; b|] -> a, b
            | [|a|] -> a, 0
            | _ -> failwith "unexpected"
        let file = { ID = id; Idx = idx; Size = fileSize }
        idx <- idx + fileSize + freeSpace
        file
    )

    let mutable start = 0
    let mutable cur = Array.length files - 1
    for curID = files[cur].ID downto 1 do
        while cur > 0 && files[cur].ID <> curID do
            cur <- cur - 1

        while start < cur && files[start + 1].Idx - (files[start].Idx + files[start].Size) = 0 do
            start <- start + 1

        let mutable i = start
        while i < cur && files[i + 1].Idx - (files[i].Idx + files[i].Size) < files[cur].Size do
            i <- i + 1
        
        if i < cur then
            let file = files[cur]
            files <- files |> Array.removeAt cur
            files <- files |> Array.insertAt (i + 1) {file with Idx = files[i].Idx + files[i].Size }

    files |> Array.map (fun file -> 
        seq { 0..file.Size - 1 } |> Seq.sumBy (fun i -> int64 file.ID * int64 (file.Idx + i))
    ) |> Array.sum |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 1928)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 6382875730645L)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int64 2858)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 6420913943576L)
    ]

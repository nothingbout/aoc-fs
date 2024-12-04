module AOC2024.Day04
open Utils
open Utils.Globals

let parseMap lines : Array2<char> = 
    Array2.fromStringLines lines
    
let getChars pos dir i j map = 
    seq { i..j } |> Seq.map (fun i -> 
        map |> Array2.get (pos + (dir * i)) '.')

let seqMatch a b = 
    Seq.zip a b |> Seq.forall (fun (a, b) -> a = b)

let solveP1 (inputLines: string list) = 
    let map = inputLines |> parseMap
    map |> Array2.mapi (fun pos _ -> 
        Vec2.dir8 |> Seq.filter (fun dir -> 
            map |> getChars pos dir 0 3 |> seqMatch "XMAS"
        ) |> Seq.length
    ) |> Array2.sum |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let dirs = Vec2.diag4 |> Array.ofSeq
    map |> Array2.mapi (fun pos _ -> 
        seq { 0..3 } |> Seq.filter (fun i -> 
            map |> getChars pos (dirs[i]) -1 1 |> seqMatch "MAS" &&
            map |> getChars pos (dirs[(i + 1) % 4]) -1 1 |> seqMatch "MAS"
        ) |> Seq.length
    ) |> Array2.sum |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 18)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 2639)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 9)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 2005)
    ]

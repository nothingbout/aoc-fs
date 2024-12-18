module AOC2024.Day18
open Utils
open Utils.Globals

let parsePosition line = 
    match line |> String.splitByString "," with
    | [a; b] -> Vec2.make (int a) (int b)
    | _ -> failwith "unexpected"

let parseMap lines = 
    lines |> List.mapi (fun idx line -> 
        (parsePosition line, idx)
    ) |> Map.ofList

let bfs bounds map n startPos endPos = 
    let found = [startPos] |> Search.bfs (fun pos dist ->
        if pos = endPos then Search.StopSearch else
        Vec2.dir4 
        |> Seq.map (fun dir -> (pos + dir, dist + 1))
        |> Seq.filter (fun (pos, _) -> 
            Rect.contains pos bounds
            && match Map.tryFind pos map with Some i -> i >= n | None -> true
        )
        |> Search.Neighbors
    )
    if Map.containsKey endPos found then Some found else None

let getDistance endPos found = 
    Map.find endPos found |> fun node -> Search.PathNode.distance node

let getLowestIdxOnPath map endPos found = 
    let path = Search.getFirstPath endPos found
    path |> List.choose (fun pos -> Map.tryFind pos map) |> List.min

let solveP1 n dims (inputLines: string list) = 
    let bounds = IntRect.withSize (Vec2.make 0 0) dims
    let map = inputLines |> parseMap
    let startPos = Vec2.make 0 0
    let endPos = dims - Vec2.make 1 1
    bfs bounds map n startPos endPos |> Option.get |> getDistance endPos |> Answer.int
    
let solveP2 dims (inputLines: string list) = 
    let bounds = IntRect.withSize (Vec2.make 0 0) dims
    let map = inputLines |> parseMap
    let startPos = Vec2.make 0 0
    let endPos = dims - Vec2.make 1 1

    let mutable idx = 0
    let mutable impassable = false
    while not impassable do
        match bfs bounds map (idx + 1) startPos endPos with
        | Some found -> idx <- getLowestIdxOnPath map endPos found
        | None -> impassable <- true

    inputLines |> List.item idx |> parsePosition |> fun pos -> $"{pos.X},{pos.Y}" |> Answer.string

let getPuzzles () = 
    [
        Puzzle.create (solveP1 12 (Vec2.make 7 7)) "Part 1" "example.txt" (Answer.int 22)
        Puzzle.create (solveP1 1024 (Vec2.make 71 71)) "Part 1" "input.txt" (Answer.int 268)
        Puzzle.create (solveP2 (Vec2.make 7 7)) "Part 2" "example.txt" (Answer.string "6,1")
        Puzzle.create (solveP2 (Vec2.make 71 71)) "Part 2" "input.txt" (Answer.string "64,11")
    ]

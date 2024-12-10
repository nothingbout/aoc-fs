module AOC2024.Day10
open Utils
open Utils.Globals

let parseMap lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                yield (Vec2.make x y, int (c - '0'))
    } |> Map.ofSeq

let isGoodTrail map pos npos = 
    (map |> Map.get npos -1) = (map |> Map.get pos -1) + 1

let rec dfs (expand : Vec2<int> -> Vec2<int> seq) visited pos = 
    let visited = visited |> Set.add pos
    for npos in expand pos do
        if not (visited |> Set.contains npos) then 
            (visited, npos) ||> dfs expand
        else ()

let solveP1 (inputLines: string list) = 
    let map = inputLines |> parseMap
    Map.keys map |> Seq.filter (fun pos -> map[pos] = 0)
    |> Seq.map (fun startPos ->
        let reachable = [startPos] |> Search.bfs (fun pos dist -> 
            Vec2.dir4 |> Seq.map (fun dir -> pos + dir, dist + 1)
            |> Seq.filter (fun (npos, _) -> isGoodTrail map pos npos) 
            |> Search.Neighbors
        )
        reachable |> Map.keys |> Seq.filter (fun pos -> map[pos] = 9) |> Seq.length
    )
    |> Seq.sum |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap

    let mutable totalCount = 0
    for startPos in Map.keys map |> Seq.filter (fun pos -> map[pos] = 0) do
        (Set.empty, startPos) ||> dfs (fun pos -> 
            if map[pos] = 9 then totalCount <- totalCount + 1

            Vec2.dir4 |> Seq.map (fun dir -> pos + dir) 
            |> Seq.filter (fun npos -> isGoodTrail map pos npos) 
        )
    Answer.int totalCount

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 36)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 798)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 81)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 1816)
    ]

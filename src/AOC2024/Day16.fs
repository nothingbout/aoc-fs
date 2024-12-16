module AOC2024.Day16
open Utils
open Utils.Globals

let dijkstra map startPos = 
    [((startPos, '>'), 0)] |> Search.dijkstra (fun (pos, dir) dist ->
        seq {
            if not (map |> Set.contains (pos + GridDir.toVec dir)) then 
                yield (pos + GridDir.toVec dir, dir), dist + 1
            yield (pos, GridDir.turnLeft dir), dist + 1000
            yield (pos, GridDir.turnRight dir), dist + 1000
        } |> Search.Neighbors
    )

let minDistance endPos found = 
    GridDir.all |> Seq.map (fun dir -> 
        match Map.tryFind (endPos, dir) found with
        | Some node -> Search.PathNode.distance node
        | None -> 1000000000
    ) |> Seq.min

let solveP1 (inputLines: string list) = 
    let map = inputLines |> GridMap.parseMapSet '#'
    let startPos = inputLines |> GridMap.parseMapToken 'S'
    let endPos = inputLines |> GridMap.parseMapToken 'E'
    dijkstra map startPos |> minDistance endPos |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> GridMap.parseMapSet '#'
    let startPos = inputLines |> GridMap.parseMapToken 'S'
    let endPos = inputLines |> GridMap.parseMapToken 'E'

    let found = dijkstra map startPos
    let minDist = minDistance endPos found

    (Set.empty, GridDir.all)
    ||> Seq.fold (fun seats dir ->
        match Map.tryFind (endPos, dir) found with 
        | Some node when node.Distance = minDist ->
            let fromPos = Search.allFromPositions (endPos, dir) found
            Set.union seats (fromPos |> Set.map (fun p -> fst p))
        | _ -> seats
    )
    |> Set.count |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 7036)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 122492)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 45)
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.int 64)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 520)
    ]

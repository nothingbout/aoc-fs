module AOC2024.Day20
open Utils

let bfs map (startPos : Vec2<int>) = 
    [startPos] 
    |> Search.bfs (fun pos dist ->
        Vec2.dir4 
        |> Seq.map (fun dir -> (pos + dir, dist + 1))
        |> Seq.filter (fun (pos, _) -> not (map |> Set.contains pos))
        |> Search.Neighbors
    )

let distanceToNode = Search.PathNode.distance

let makeCheatJumps maxCheatLength = 
    [for y = -maxCheatLength to maxCheatLength do
        for x = -maxCheatLength to maxCheatLength do
            if (x <> 0 || y <> 0) && abs x + abs y <= maxCheatLength then
                yield Vec2.make x y]

let findCheats cheatJumps minSavings noCheatDist bfsFromStart bfsFromEnd = 
    Seq.allPairs (bfsFromStart |> Map.toSeq) cheatJumps |> Seq.choose (fun ((pos, node), jump) ->
        match Map.tryFind (pos + jump) bfsFromEnd with
        | Some other -> 
            let savings = noCheatDist - (Vec2.abssum jump + distanceToNode node + distanceToNode other)
            if savings >= minSavings then Some savings
            else None
        | None -> None
    )

let solve maxCheatLength minSavings (inputLines: string list) = 
    let map = inputLines |> GridMap.parseMapSet '#'
    let startPos = inputLines |> GridMap.parseMapToken 'S'
    let endPos = inputLines |> GridMap.parseMapToken 'E'

    let bfsFromStart = bfs map startPos
    let bfsFromEnd = bfs map endPos

    let noCheatDist = 
        Map.find endPos bfsFromStart 
        |> fun node -> distanceToNode node

    let cheats = findCheats maxCheatLength minSavings noCheatDist bfsFromStart bfsFromEnd
    // cheats |> Seq.countBy id |> Seq.sortBy fst |> inspect |> ignore
    cheats |> Seq.length |> Answer.int

let solveP1 minSavings (inputLines: string list) = 
    solve (makeCheatJumps 2) minSavings inputLines

let solveP2 minSavings (inputLines: string list) = 
    solve (makeCheatJumps 20) minSavings inputLines

let getPuzzles () = 
    [
        Puzzle.create (solveP1 1) "Part 1" "example.txt" (Answer.int 44)
        Puzzle.create (solveP1 100) "Part 1" "input.txt" (Answer.int 1497)
        Puzzle.create (solveP2 50) "Part 2" "example.txt" (Answer.int 285)
        Puzzle.create (solveP2 100) "Part 2" "input.txt" (Answer.int 1030809)
    ]

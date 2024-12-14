module AOC2024.Day14
open Utils
open Utils.Globals

[<Struct>]
type Robot = { Pos : Vec2<int>; Vel : Vec2<int> }

let parseRobot line = 
    line |> Substr.ofStr |> Scan.scan {
        //p=62,57 v=-91,42
        do! Scan.skipString "p="
        let! x = Scan.takeInt
        do! Scan.skipString ","
        let! y = Scan.takeInt
        do! Scan.skipString " v="
        let! vx = Scan.takeInt
        do! Scan.skipString ","
        let! vy = Scan.takeInt
        return { Pos = Vec2.make x y; Vel = Vec2.make vx vy }
    } |> Scan.finish

let wrap dims pos = 
    Vec2.make (pos.X %+ dims.X) (pos.Y %+ dims.Y)

let step dims robots = 
    robots |> List.map (fun robot ->
        { robot with Pos = (robot.Pos + robot.Vel) |> wrap dims }
    )

let quadrant (dims : Vec2<_>) pos = 
    let mid = dims / 2
    if pos.X < mid.X then
        if pos.Y < mid.Y then 1
        else if pos.Y > mid.Y then 3
        else 0
    else if pos.X > mid.X then
        if pos.Y < mid.Y then 2
        else if pos.Y > mid.Y then 4
        else 0
    else 0

let visualize seconds dims robots = 
    printfn $"SECONDS: {seconds}"
    robots
    |> List.map (fun robot -> (robot.Pos, "#"))
    |> Map.ofList
    |> GridMap.toLines (IntRect.withSize (Vec2.make 0 0) dims) "." 
    |> GridMap.printLines

let adjacencyScore robots = 
    robots 
    |> List.map (fun robot -> robot.Pos)
    |> List.sortBy (fun pos -> pos.Y * 1000 + pos.X )
    |> List.pairwise
    |> List.sumBy (fun (a, b) -> if a.Y = b.Y && a.X + 1 = b.X then 1 else 0)

let solveP1 dims (inputLines: string list) = 
    let mutable robots = inputLines |> List.map parseRobot
    for i = 1 to 100 do 
        robots <- step dims robots
    robots 
    |> List.groupBy (fun robot -> quadrant dims robot.Pos)
    |> List.filter (fun (q, _) -> q <> 0)
    |> List.map (fun (_, robots) -> List.length robots)
    |> List.reduce ( * )
    |> Answer.int
    
let solveP2 dims (inputLines: string list) = 
    let mutable robots = inputLines |> List.map parseRobot
    seq {
        for i = 1 to 100000 do 
            robots <- step dims robots

            let r = adjacencyScore robots
            if r > 100 then
                robots |> visualize i dims
                yield i
    }
    |> Seq.head
    |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create (solveP1 (Vec2.make 11 7)) "Part 1" "example.txt" (Answer.int 12)
        Puzzle.create (solveP1 (Vec2.make 101 103)) "Part 1" "input.txt" (Answer.int 229839456)
        Puzzle.create (solveP2 (Vec2.make 101 103)) "Part 2" "input.txt" (Answer.int 7138)
    ]

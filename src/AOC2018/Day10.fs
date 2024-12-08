module AOC2018.Day10
open Utils
open Utils.Globals

[<Struct>]
type Star = { Pos : Vec2<int>; Vel : Vec2<int> }

let parseStar line = 
    line |> Substr.ofStr |> Scan.scan {
        // position=< 9,  1> velocity=< 0,  2>
        do! Scan.skipString "position=<"
        do! Scan.skipSpaces
        let! px = Scan.takeInt
        do! Scan.skipString ","
        do! Scan.skipSpaces
        let! py = Scan.takeInt
        do! Scan.skipString "> velocity=<"
        do! Scan.skipSpaces
        let! vx = Scan.takeInt
        do! Scan.skipString ","
        do! Scan.skipSpaces
        let! vy = Scan.takeInt
        do! Scan.skipString ">"
        return { Pos = Vec2.make px py; Vel = Vec2.make vx vy }
    } |> Scan.finish

let starsBounds stars =
    stars |> List.map (fun s -> s.Pos) |> Rect.encapsulating

let rec stepUntilWeHaveText textHeight stars step = 
    let bounds = stars |> starsBounds
    if IntRect.height bounds <= textHeight then stars, step
    else
    let newStars = 
        stars 
        |> List.map (fun { Pos = pos; Vel = vel } -> { Pos = pos + vel; Vel = vel }) 
        |> List.filter (fun star ->  Rect.contains star.Pos bounds)    
    (newStars, step + 1) ||> stepUntilWeHaveText textHeight

let solveP1 textHeight (inputLines: string list) = 
    let stars = inputLines |> List.map parseStar
    let finalStars, _ = (stars, 0) ||> stepUntilWeHaveText textHeight
    let lines = finalStars |> List.map (fun star -> star.Pos, "#") |> Map.ofList |> GridMap.toLinesAutoBounds "."
    lines |> GridMap.printLines
    lines |> List.length |> Answer.int
    
let solveP2 textHeight (inputLines: string list) = 
    let stars = inputLines |> List.map parseStar
    let _, steps = (stars, 0) ||> stepUntilWeHaveText textHeight
    Answer.int steps

let getPuzzles () = 
    [
        Puzzle.create (solveP1 8) "Part 1" "example.txt" (Answer.int 8)
        Puzzle.create (solveP1 10) "Part 1" "input.txt" (Answer.int 10)
        Puzzle.create (solveP2 8) "Part 2" "example.txt" (Answer.int 3)
        Puzzle.create (solveP2 10) "Part 2" "input.txt" (Answer.int 10612)
    ]

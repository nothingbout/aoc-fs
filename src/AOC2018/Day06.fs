module AOC2018.Day06
open Utils
open Utils.Globals

let parsePoint line = 
    match line |> Str.splitByString ", " |> List.map int with
    | [x; y] -> Vec2.make x y
    | _ -> failwith $"not a valid point {line}"

let manhattan a b = 
    b - a |> Vec2.abs |> Vec2.sum

let isOnEdge rect point =
    point.X = rect.XR.Start || point.X = rect.XR.Finish || 
    point.Y = rect.YR.Start || point.Y = rect.YR.Finish

let solveP1 (inputLines: string list) = 
    let points = inputLines |> List.map parsePoint
    let bounds = Rect.encapsulating points
    let nearestCounts = System.Collections.Generic.Dictionary<_, _>()

    for p in bounds |> IntRect.pointsSeq do
        let nearest = points |> List.minBy (fun p2 -> manhattan p p2)
        let nd = manhattan p nearest
        if points |> Seq.filter (fun p2 -> manhattan p p2 = nd) |> Seq.length = 1 then
            if isOnEdge bounds p then
                nearestCounts[nearest] <- -1
            else 
                nearestCounts[nearest] <- 
                    match nearestCounts.TryGetValue nearest with 
                    | true, count when count < 0 -> -1
                    | true, count -> count + 1 
                    | _ -> 1

    nearestCounts :> seq<_> |> Seq.map (|KeyValue|) |> Seq.map snd |> Seq.max |> Answer.int
    
let solveP2 maxTotalDistance (inputLines: string list) = 
    let points = inputLines |> List.map parsePoint
    let bounds = Rect.encapsulating points

    bounds |> IntRect.pointsSeq 
    |> Seq.filter (fun p -> points |> List.sumBy (manhattan p) <= maxTotalDistance)
    |> Seq.length |> Answer.int

let getPuzzles() = 
    "aoc2018/day06", [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 17)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 4233)
        Puzzle.create (solveP2 31) "Part 2" "example.txt" (Answer.int 16)
        Puzzle.create (solveP2 9999) "Part 2" "input.txt" (Answer.int 45290)
    ]




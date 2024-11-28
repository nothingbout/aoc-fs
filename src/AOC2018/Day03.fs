module AOC2018.Day03
open Utils
open Utils.Globals

type Claim = { ID : int; Rect : Rect<int> }

let parseClaim (line : string) : Claim = 
    // #1264 @ 30,187: 11x20
    line |> ScanSeq.ofString |> Scan.scan {
        do! Scan.skipString "#"
        let! id = Scan.int
        do! Scan.skipString " @ "
        let! x = Scan.int
        do! Scan.skipString ","
        let! y = Scan.int
        do! Scan.skipString ": "
        let! w = Scan.int
        do! Scan.skipString "x"
        let! h = Scan.int
        let rect = IntRect.withSize (Vec2.make x y) (Vec2.make w h)
        return {ID = id; Rect = rect}
    } |> Scan.finish

// let solveP1 (inputLines: string list) = 
//     let claims = inputLines |> List.map parseClaim
//     let pointsSeq = 
//         Seq.allPairs claims claims |> Seq.filter (fun (a, b) -> a.ID < b.ID)
//         |> Seq.choose (fun (a, b) -> 
//             match Rect.intersect a.Rect b.Rect with
//             | None -> None
//             | Some rect -> Some <| IntRect.pointsSeq rect
//         )
//         |> Seq.concat 
//     let points = System.Collections.Generic.HashSet(pointsSeq, HashIdentity.Structural)
//     Answer.int points.Count

let solveP1 (inputLines: string list) = 
    let claims = inputLines |> List.map parseClaim |> Array.ofList
    let points = System.Collections.Generic.HashSet(HashIdentity.Structural)
    for i = 0 to Array.length claims - 1 do
        for j = i + 1 to Array.length claims - 1 do
            match Rect.intersect claims[i].Rect claims[j].Rect with
            | None -> ()
            | Some rect -> 
                for p in IntRect.pointsSeq rect do
                    points.Add(p) |> ignore
    Answer.int points.Count

let solveP2 (inputLines: string list) = 
    let claims = inputLines |> List.map parseClaim
    claims |> Seq.find (fun a -> 
        None = (claims |> Seq.tryFind (fun b -> a.ID <> b.ID && Rect.intersect a.Rect b.Rect <> None))
    ) |> fun c -> Answer.int c.ID

let getPuzzles() = 
    "aoc2018/day03", [
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 104126)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 695)
    ]




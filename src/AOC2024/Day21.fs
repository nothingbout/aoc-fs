module AOC2024.Day21
open Utils

let parseCode line = 
    line |> List.ofSeq

let makeKeypad line = 
    line |> String.splitByString "\n" |> GridMap.ofLines
    |> Map.toSeq |> Seq.map (fun (k, v) -> (v, k)) |> Map.ofSeq

let numericKeypad = "789\n456\n123\nX0A" |> makeKeypad
let arrowKeypad = "X^A\n<v>" |> makeKeypad

let straightPath startPos nextPos = 
    if startPos.Y = nextPos.Y then
        List.replicate (abs (nextPos.X - startPos.X)) (if nextPos.X < startPos.X then '<' else '>') 
    else if startPos.X = nextPos.X then 
        List.replicate (abs (nextPos.Y - startPos.Y)) (if nextPos.Y < startPos.Y then '^' else 'v') 
    else
        failwith "unexpected"

let pathsToPressChar (keypad : Map<char, Vec2<int>>) startChar nextChar =
    let startPos = Map.find startChar keypad
    let nextPos = Map.find nextChar keypad

    if startPos.X = nextPos.X || startPos.Y = nextPos.Y then [straightPath startPos nextPos @ ['A']] else
    let c1 = Vec2.make startPos.X nextPos.Y
    let c2 = Vec2.make nextPos.X startPos.Y
    let path1 = straightPath startPos c1 @ straightPath c1 nextPos @ ['A']
    let path2 = straightPath startPos c2 @ straightPath c2 nextPos @ ['A']
    if Map.find 'X' keypad = c1 then [path2]
    else if Map.find 'X' keypad = c2 then [path1]
    else [path1; path2]

let rec rootPathLengthBetweenChars mem keypad nrobots startChar nextChar = 
    let recurse = Memoize.run3 mem (rootPathLengthBetweenChars mem arrowKeypad)
    if nrobots = 0 then 
        Vec2.abssum (Map.find nextChar keypad - Map.find startChar keypad) + 1 |> int64 
    else
        let paths = pathsToPressChar keypad startChar nextChar
        paths |> List.map (fun path ->
            'A' :: path |> List.pairwise |> List.sumBy (fun (c1, c2) -> recurse (nrobots - 1) c1 c2)
        ) |> List.min

let findCodeRootPathLength nrobots code = 
    let mem = Memoize.init ()
    'A' :: code |> List.pairwise |> List.map (fun (startChar, nextChar) ->
        rootPathLengthBetweenChars mem numericKeypad nrobots startChar nextChar
    ) |> List.sum

let getComplexity code pathLength = 
    let numeric = List.take 3 code |> String.ofSeq |> int
    pathLength * int64 numeric

let solveP1 (inputLines: string list) = 
    let codes = inputLines |> List.map parseCode
    codes |> Seq.map (fun code -> 
        // getComplexity code (findCombinedPath 2 code |> List.length |> int64)
        getComplexity code (findCodeRootPathLength 2 code)
    ) |> Seq.sum |> Answer.int64
    
let solveP2 (inputLines: string list) = 
    let codes = inputLines |> List.map parseCode
    codes |> Seq.map (fun code -> 
        getComplexity code (findCodeRootPathLength 25 code)
    ) |> Seq.sum |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 126384)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 157908)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 196910339808654L)
    ]

///// Initial solution for part 1

// let rec findPaths (keypad : Map<char, Vec2<int>>) startChar chars (paths : char list list) =
//     match chars with
//     | nextChar :: chars ->
//         let paths = 
//             paths |> List.map (fun path ->
//                 pathsToPressChar keypad startChar nextChar |> List.map (fun p -> p @ path)
//             ) |> List.concat
//         findPaths keypad nextChar chars paths
//     | _ -> paths

// let findCombinedPath nrobots code = 
//     let mutable paths = findPaths numericKeypad 'A' code [[]]
//     for i = 1 to nrobots do
//         paths <- paths |> List.map (fun path -> findPaths arrowKeypad 'A' path [[]]) |> List.concat
//     paths |> List.minBy (fun path -> List.length path)

///// Debug code

// let rec applyPath keypad startPos path nextPath = 
//     match path with
//     | 'A' :: path -> 
//         let code = Map.findKey (fun _ v -> v = startPos) keypad :: nextPath
//         applyPath keypad startPos path code
//     | dir :: path ->
//         applyPath keypad (startPos + GridDir.toVec dir) path nextPath
//     | _ -> List.rev nextPath

// let pathToString path = 
//     path |> List.map toString |> String.concat ""

// let debugPath depth path = 
//     let mutable path = path
//     let mutable depth = depth
//     printfn $"depth {depth}: {pathToString path} {List.countBy id path |> toInspectString}"
//     while depth > 0 do
//         path <- applyPath arrowKeypad (Map.find 'A' arrowKeypad) path []
//         depth <- depth - 1
//         printfn $"depth {depth}: {pathToString path}"

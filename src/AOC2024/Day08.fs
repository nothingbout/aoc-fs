module AOC2024.Day08
open Utils
open Utils.Globals

let parseMap lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                yield (Vec2.make x y, c)
    } |> Map.ofSeq

let rec allPositionsInLine map start delta = 
    if not (Map.containsKey start map) then []
    else start :: allPositionsInLine map (start + delta) delta

let allAntinodes generator map = 
    map |> Map.toSeq |> Seq.filter (fun (_, c) -> c <> '.') |> Seq.groupByAndMap snd fst |> Seq.map (fun (_, positions) ->
        Seq.allPairs positions positions |> Seq.filter (fun (a, b) -> a <> b) |> Seq.map (fun (a, b) ->
            (generator map a b) @ (generator map b a)
        ) |> Seq.concat
    ) |> Seq.concat 
    |> Set.ofSeq

let solveP1 (inputLines: string list) = 
    parseMap inputLines
    |> allAntinodes (fun map a b -> let c = a + (a - b) in if Map.containsKey c map then [c] else [])
    |> Set.count |> Answer.int
    
let solveP2 (inputLines: string list) = 
    parseMap inputLines 
    |> allAntinodes (fun map a b -> allPositionsInLine map a (a - b))
    |> Set.count |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 14)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 341)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 34)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 1134)
    ]

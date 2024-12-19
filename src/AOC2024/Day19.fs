module AOC2024.Day19
open Utils

let parseInput lines = 
    let towels = lines |> List.item 0 |> String.splitByString ", "
    let designs = lines |> List.skip 2
    towels, designs

let rec isPossible (towels : string list) (design : Substr) =
    if Substr.isEmpty design then true else
    towels |> List.exists (fun towel -> 
        if not (Substr.startsWith towel design) then false else
        isPossible towels (Substr.trimPrefix towel design)
    )

let rec countWays mem (towels : string list) (design : Substr) =
    let countWays a = Memoize.run mem (countWays mem towels) a
    if Substr.isEmpty design then 1L else
    towels |> List.sumBy (fun towel -> 
        if not (Substr.startsWith towel design) then 0L else
        countWays (Substr.trimPrefix towel design)
    )

let solveP1 (inputLines: string list) = 
    let towels, designs = inputLines |> parseInput
    designs |> List.filter (fun design -> 
        isPossible towels (Substr.ofStr design)
    ) |> List.length |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let towels, designs = inputLines |> parseInput
    let mem = Memoize.init ()
    designs |> List.map (fun design -> 
        countWays mem towels (Substr.ofStr design)
    ) |> List.sum |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 6)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 315)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int64 16)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 625108891232249L)
    ]

module AOC2024.Day05
open Utils
open Utils.Globals

let parseRule line = 
    match line |> String.splitByString "|" with 
    | [x; y] -> (int x, int y)
    | _ -> failwith "unexpected"

let rec parseRules rules lines = 
    match lines with
    | line :: _ when line = "" -> lines, rules |> Seq.groupByIntoMap fst snd
    | line :: lines -> lines |> parseRules (parseRule line :: rules)
    | _ -> failwith "unexpected"

let parseUpdate line = 
    line |> String.splitByString "," |> List.map int |> Array.ofList

let parseInput lines = 
    let lines, rules = lines |> parseRules []
    let updates = lines |> List.skip 1 |> List.map parseUpdate
    rules, updates

let sortByRules rules update = 
    update |> Array.sortWith (fun x y ->
        if rules |> Map.get x Seq.empty |> Seq.contains y then -1
        else if rules |> Map.get y Seq.empty |> Seq.contains x then 1
        else 0
    )

let isInCorrectOrder rules update = 
    update |> sortByRules rules |> Seq.forall2 (=) update

let mid update = 
    update |> Array.item (Array.length update / 2)

let solveP1 (inputLines: string list) = 
    let rules, updates = inputLines |> parseInput
    updates |> List.filter (isInCorrectOrder rules) |> List.sumBy mid |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let rules, updates = inputLines |> parseInput
    updates |> List.filter (isInCorrectOrder rules >> not) 
    |> List.map (sortByRules rules) |> List.sumBy mid |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 143)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 6612)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 123)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 4944)
    ]

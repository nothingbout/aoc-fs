module AOC2024.Day01
open Utils
open Utils.Globals

let parseEntry line = 
    match line |> Str.splitByString "   " with 
    | [a; b] -> (int a, int b)
    | _ -> failwith "unexpected"

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> List.map parseEntry
    let list1, list2 = entries |> List.unzip
    (List.sort list1, List.sort list2) ||> List.zip |> List.sumBy (fun (a, b) -> abs(a - b)) |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let entries = inputLines |> List.map parseEntry
    let list1, list2 = entries |> List.unzip
    let counts2 = list2 |> List.countBy id |> Map.ofList
    list1 |> List.sumBy (fun a -> a * (counts2 |> Map.get a 0)) |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 11)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 3508942)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 31)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 26593248)
    ]

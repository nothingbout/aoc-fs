module AOC2018.Day05
open Utils
open Utils.Globals

let inline isLower a = 'a' <= a && a <= 'z'
let inline isUpper a = 'A' <= a && a <= 'Z'

let inline toLower a = 
    if isLower a then a
    else if isUpper a then a + ('a' - 'A')
    else failwith $"unexpected character {a}"

let shouldReact a b = 
    toLower a = toLower b && isLower a <> isLower b

let rec react (xs : char list) (ys : char list) : char list =
    match xs, ys with
    | xs, [] -> List.rev xs
    | [], y :: ys -> react [y] ys
    | x :: xs, y :: ys -> 
        if shouldReact x y then 
            react xs ys
        else
            react (y :: x :: xs) ys


let solveP1 (inputLines: string list) = 
    let units = inputLines |> List.head |> List.ofSeq
    react [] units |> List.length |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let units = inputLines |> List.head |> List.ofSeq
    seq { 'a'..'z' } |> Seq.map (fun remove -> 
        let units = units |> List.filter (fun c -> toLower c <> remove)
        react [] units |> List.length
        )
    |> Seq.min |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 10)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 10708)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 4)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 5330)
    ]




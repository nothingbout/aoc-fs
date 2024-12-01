module AOC2018.Day02 
open Utils

let hasCountOfChars count (str : string) = 
    match str.ToCharArray() |> Array.sort |> Array.groupBy (fun c -> c) 
        |> Array.tryFind (fun (_, items) -> items.Length = count) with
    | None -> false
    | Some _ -> true

let commonChars (xs : string) (ys : string) = 
    Array.zip (xs.ToCharArray()) (ys.ToCharArray()) 
    |> Array.filter (fun (x, y) -> x = y) 
    |> Array.map (fun (x, _) -> x) 
    |> System.String

let solveP1 (inputLines: string list) = 
    let two = (inputLines |> List.filter (hasCountOfChars 2) |> List.length)
    let three = (inputLines |> List.filter (hasCountOfChars 3) |> List.length)
    Answer.int (two * three)

let solveP2 (inputLines: string list) = 
    List.allPairs inputLines inputLines 
    |> List.find (fun (xs, ys) -> String.length xs - String.length (commonChars xs ys) = 1)
    |> fun (xs, ys) -> commonChars xs ys 
    |> Answer.string

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 5976)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.string "xretqmmonskvzupalfiwhcfdb")
    ]


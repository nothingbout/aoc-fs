module AOC2018.Day08
open Utils
open Utils.Globals

type Input = { Data : int list }

let parseInput line = 
    line |> String.splitByString " " |> List.map int |> fun ints -> {Data = ints}

let rec computeNodeSum data =
    match data with 
    | numChildren :: numMetadata :: data -> 
        let mutable ptr = data
        let mutable sum = 0
        for _ in 1..numChildren do
            let (d, s) = computeNodeSum ptr
            ptr <- d
            sum <- sum + s
        for _ in 1..numMetadata do
            sum <- sum + List.head ptr
            ptr <- List.tail ptr
        (ptr, sum)
    | _ -> failwith "corrupt data"

let rec computeNodeValue data =
    match data with 
    | 0 :: _ -> computeNodeSum data
    | numChildren :: numMetadata :: data -> 
        let mutable ptr = data
        let mutable values = Array.zeroCreate numChildren
        for i in 1..numChildren do
            let (d, v) = computeNodeValue ptr
            ptr <- d
            Array.set values (i - 1) v
        let mutable sum = 0
        for _ in 1..numMetadata do
            let i = List.head ptr
            ptr <- List.tail ptr
            if i >= 1 && i <= numChildren then
                sum <- sum + Array.get values (i - 1)
        (ptr, sum)
    | _ -> failwith "corrupt data"

let solveP1 (inputLines: string list) = 
    let input = inputLines |> List.head |> parseInput
    let (_, sum) = computeNodeSum input.Data
    Answer.int sum
    
let solveP2 (inputLines: string list) = 
    let input = inputLines |> List.head |> parseInput
    let (_, sum) = computeNodeValue input.Data
    Answer.int sum

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 138)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 36566)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 66)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 30548)
    ]

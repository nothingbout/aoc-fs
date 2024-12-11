module AOC2024.Day11
open Utils
open Utils.Globals

let parseStones line = 
    line |> String.extractDigitGroups |> List.map int64

let changeStone stone = 
    if stone = 0L then [1L]
    else
    let digits = countDigitsInt64 stone
    if digits % 2 = 0 then
        let mask = pown 10L (digits / 2)
        [stone / mask; stone % mask]
    else
        [stone * 2024L]

let solveP1 (inputLines: string list) = 
    let stones = inputLines |> List.head |> parseStones
    let mutable stones = stones
    for i = 1 to 25 do
        let newStones = stones |> List.map changeStone |> List.concat
        stones <- newStones
    List.length stones |> Answer.int

let solveP2 (inputLines: string list) = 
    let stones = inputLines |> List.head |> parseStones
    let mutable stones = stones |> List.map (fun stone -> (stone, 1L))
    for i = 1 to 75 do
        let newStones = 
            stones |> List.map (fun (stone, count) -> 
                changeStone stone |> List.map (fun stone -> (stone, count))
            ) |> List.concat
            |> List.groupByMapAndFold fst snd (+) 0L
        stones <- newStones
    stones |> List.sumBy snd |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 55312)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 198075)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 235571309320764L)
    ]

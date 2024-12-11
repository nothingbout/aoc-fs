module AOC2024.Day11
open Utils
open Utils.Globals

let parseStones line = 
    line |> String.extractDigitGroups |> List.map int64

let rec count mem n stone = 
    let count = Memoize.run2 mem (count mem) (n - 1)
    if n = 0 then 1L
    else if stone = 0L then 
        count 1L
    else
    let digits = countDigitsInt64 stone
    if digits % 2 = 1 then 
        count (stone * 2024L)
    else
        let mask = pown 10L (digits / 2)
        count (stone / mask) + 
        count (stone % mask)

let solveP1 (inputLines: string list) = 
    let stones = inputLines |> List.head |> parseStones
    let mem = Memoize.init ()
    stones |> List.sumBy (count mem 25) |> Answer.int64

let solveP2 (inputLines: string list) = 
    let stones = inputLines |> List.head |> parseStones
    let mem = Memoize.init ()
    stones |> List.sumBy (count mem 75) |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 55312)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 198075)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 235571309320764L)
    ]

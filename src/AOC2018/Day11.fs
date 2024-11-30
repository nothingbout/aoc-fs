module AOC2018.Day11
open Utils
open Utils.Globals

type Input = { Serial : int }

let parseInput line = 
    line |> ScanSeq.ofString |> Scan.scan {
        let! serial = Scan.positiveInt
        return { Serial = serial }
    } |> Scan.finish

let powerLevel serial pos = 
    let rackID = pos.X + 10
    (rackID * pos.Y + serial) * rackID / 100 % 10 - 5

let testPowerLevel serial pos expected = 
    let p = powerLevel serial pos
    if p <> expected then failwith $"{serial} {pos} -> {p} but expected {expected}"

let testPowerLevels () = 
    testPowerLevel 8 (Vec2.make 3 5) 4
    testPowerLevel 57 (Vec2.make 122 79) -5
    testPowerLevel 39 (Vec2.make 217 196) 0
    testPowerLevel 71 (Vec2.make 101 153) 4

let cumulativePowerLevels serial gridSize : Array2<int> = 
    let arr = gridSize + Vec2.make 1 1 |> Array2.zeroCreate
    for y = 1 to gridSize.Y do
        let mutable colSum = 0
        for x = 1 to gridSize.X do
            let pos = Vec2.make x y
            colSum <- colSum + powerLevel serial pos
            arr[pos] <- arr[Vec2.make x (y - 1)] + colSum
    arr

let squarePower (cumPowerLevels : Array2<int>) squareSize start = 
    let finish = start + Vec2.make (squareSize - 1) (squareSize - 1)
    cumPowerLevels[finish] 
    - cumPowerLevels[Vec2.make finish.X (start.Y - 1)]
    - cumPowerLevels[Vec2.make (start.X - 1) finish.Y]
    + cumPowerLevels[Vec2.make (start.X - 1) (start.Y - 1)]

let highestPowerSquareOrigin cumPowerLevels gridSize squareSize = 
    IntRect.withSize (Vec2.make 1 1) (gridSize - Vec2.make (squareSize - 1) (squareSize - 1)) 
    |> IntRect.pointsByRow 
    |> Seq.maxBy (fun pos -> pos |> squarePower cumPowerLevels squareSize)

let solve serial minSquareSize maxSquareSize = 
    let gridSize = Vec2.make 300 300
    let cumPowerLevels = cumulativePowerLevels serial gridSize
    seq {
        for squareSize in minSquareSize..maxSquareSize do
            yield (squareSize, highestPowerSquareOrigin cumPowerLevels gridSize squareSize)
    } |> Seq.maxBy (fun (squareSize, pos) -> squarePower cumPowerLevels squareSize pos)

let solveP1 (inputLines: string list) = 
    testPowerLevels ()
    let input = inputLines |> List.head |> parseInput
    let _, pos = solve input.Serial 3 3
    Answer.string $"{pos.X},{pos.Y}"
    
let solveP2 (inputLines: string list) = 
    let input = inputLines |> List.head |> parseInput
    let squareSize, pos = solve input.Serial 1 300
    Answer.string $"{pos.X},{pos.Y},{squareSize}"

let getPuzzles() = 
    "aoc2018/day11", [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.string "33,45")
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.string "20,46")
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.string "90,269,16")
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.string "231,65,14")
    ]

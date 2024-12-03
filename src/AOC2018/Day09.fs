module AOC2018.Day09
open Utils
open Utils.Globals

type Input = { NumPlayers : int; FinalValue : int }

let parseInput line = 
    line |> Substring.ofString |> Scan.scan {
        // 10 players; last marble is worth 1618 points
        let! players = Scan.takeInt
        do! Scan.skipString " players; last marble is worth "
        let! value = Scan.takeInt
        do! Scan.skipString " points"
        return { NumPlayers = players; FinalValue = value }
    } |> Scan.finish

type Circle = { 
    XS : int list // marbles in CCW direction, the head is the "current" marble
    YS : int list // marbles in CW direction
}

module Circle = 
    let balance circle = 
        match circle with
        | { XS = []; YS = ys } -> { XS = List.rev ys; YS = [] }
        | { XS = xs; YS = [] } -> { XS = []; YS = List.rev xs }
        | _ -> failwith "don't balance unnecessarily"

    let push value circle = { XS = value :: circle.XS; YS = circle.YS }

    let rec pop circle = 
        match circle with
        | { XS = [] } -> pop (balance circle)
        | { XS = value :: xs; YS = ys } -> (value, { XS = xs; YS = ys })

    let rec rotateCCW n circle = 
        match n, circle with
        | 0, _ -> circle
        | _, { YS = [] } -> rotateCCW n (balance circle)
        | _, { XS = xs; YS = value :: ys } -> rotateCCW (n - 1) { XS = value :: xs; YS = ys }

    let rec rotateCW n circle = 
        match n, circle with
        | 0, _ -> circle
        | _, { XS = [] } -> rotateCW n (balance circle)
        | _, { XS = value :: xs; YS = ys } -> rotateCW (n - 1) { XS = xs; YS = value :: ys }

type PlayState = {
    Circle : Circle
    NextValue : int
    Scores : Map<int, int64>
}

let rec play players finalValue state = 
    let value = state.NextValue
    if value > finalValue then
        state
    else if value % 23 = 0 then
        let player = (value - 1) % players + 1
        let removedValue, circle = state.Circle |> Circle.rotateCW 7 |> Circle.pop
        let circle = circle |> Circle.rotateCCW 1
        let scores = state.Scores |> Map.change player (fun s -> Some (s.Value + int64(value + removedValue)))
        { Circle = circle; NextValue = value + 1; Scores = scores } |> play players finalValue
    else
        let circle = state.Circle |> Circle.rotateCCW 1 |> Circle.push value
        { state with Circle = circle; NextValue = value + 1 } |> play players finalValue

let winningScore numPlayers finalValue = 
    let startState = { 
        Circle = { XS = []; YS = [0] }
        NextValue = 1
        Scores = Seq.init numPlayers (fun i -> (i + 1, int64 0)) |> Map.ofSeq
    }
    let endState = play numPlayers finalValue startState
    endState.Scores |> Map.toSeq |> Seq.map snd |> Seq.max

let solveP1 (inputLines: string list) = 
    let input = inputLines |> List.head |> parseInput
    winningScore input.NumPlayers input.FinalValue |> Answer.int64
    
let solveP2 (inputLines: string list) = 
    let input = inputLines |> List.head |> parseInput
    winningScore input.NumPlayers (input.FinalValue * 100) |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example1.txt" (Answer.int64 32)
        Puzzle.create solveP1 "Part 1" "example2.txt" (Answer.int64 8317)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 399645)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 3352507536L)
    ]

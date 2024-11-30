module AOC2018.Day12
open Utils
open Utils.Globals

type InputRule = { In : bool list; Out : bool }
type Input = { Initial : bool list; Rules : InputRule list }

let inputToBoolList (str : string) =
    str :> seq<_> |> Seq.map (fun c -> match c with '#' -> true | '.' -> false | _ -> failwith $"unexpected {c}") |> List.ofSeq

let parseInitialState line = 
    line |> ScanSeq.ofString |> Scan.scan {
        // initial state: #..#.#..##......###...###
        do! Scan.skipString "initial state: "
        let! initial = Scan.all
        return initial |> inputToBoolList
    } |> Scan.finish

let parseRule line = 
    line |> ScanSeq.ofString |> Scan.scan {
        // ..#.. => .
        let! in' = Scan.take 5
        do! Scan.skipString " => "
        let! out = Scan.take 1
        return { In = in' |> inputToBoolList; Out = out |> inputToBoolList |> Seq.exactlyOne }
    } |> Scan.finish

let parseInput inputLines = 
    let initial = inputLines |> List.head |> parseInitialState
    let rules = inputLines |> List.skip 2 |> List.map parseRule
    { Initial = initial; Rules = rules }

let maskToString mask = $"%05B{mask}"

let makeMasks (pots : bool list) : int list =
    pots @ [false; false] 
    |> Seq.mapFold (fun mask p -> 
        let mask = (mask <<< 1) ||| (if p then 1 else 0) &&& 0b11111
        mask, mask) 0 
    |> fst |> Seq.skip 2 |> List.ofSeq

let makeRules inputRules = 
    inputRules 
    |> List.map (fun rule -> (makeMasks rule.In |> List.item 2, rule.Out)) 
    |> Map.ofList

let printRules (rules) = 
    rules |> Seq.iter (fun (KeyValue (mask, value)) -> printfn $"{maskToString mask} => {value}")

type PotsRow = { Indices : Range<int>; Pots : bool list }

let printPotsRow row = 
    row.Pots |> List.map (fun p -> if p then "#" else ".") |> String.concat "" |> (fun s -> printfn $"{row.Indices.Start}: {s}")

let padPots row = 
    let leftPad = 
        match row.Pots with
        | true :: _ -> [false; false]
        | false :: true :: _ -> [false]
        | _ -> []
    let rightPad = 
        match List.rev row.Pots with
        | true :: _ -> [false; false]
        | false :: true :: _ -> [false]
        | _ -> []
    { Indices = Range.make (row.Indices.Start - List.length leftPad) (row.Indices.Finish + List.length rightPad); 
        Pots = leftPad @ row.Pots @ rightPad }

let step (rules : Map<int, bool>) (row : PotsRow) = 
    let paddedRow = padPots row
    let potsMasks = makeMasks paddedRow.Pots
    let newPots = potsMasks |> List.map (fun potMask -> rules |> Map.get potMask false)
    { Indices = paddedRow.Indices; Pots = newPots }

let stepN rules n row = 
    (row, seq{1..n}) ||> Seq.fold (fun pots _ -> pots |> step rules)

let sumPlantIndices row = 
    (IntRange.toSeq row.Indices, row.Pots) ||> Seq.zip |> Seq.sumBy (fun (i, p) -> if p then i else 0)

let extrapolateSum nTarget n1 sum1 n2 sum2 =
    sum2 + (nTarget - n2) / (n2 - n1) * (sum2 - sum1)

let solveP1 (inputLines: string list) = 
    let input = inputLines |> parseInput
    let rules = makeRules input.Rules
    let initialRow = { Indices = Range.make 0 (List.length input.Initial - 1); Pots = input.Initial }
    let finalRow = initialRow |> stepN rules 20
    finalRow |> sumPlantIndices |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let input = inputLines |> parseInput
    let rules = makeRules input.Rules
    let initialRow = { Indices = Range.make 0 (List.length input.Initial - 1); Pots = input.Initial }
    let n1 = 100
    let n2 = 200
    let row1 = initialRow |> stepN rules n1
    let row2 = row1 |> stepN rules (n2 - n1)
    let finalSum = 
        extrapolateSum 50000000000L 
        <| int64 n1 <| int64 (sumPlantIndices row1)
        <| int64 n2 <| int64 (sumPlantIndices row2)
    finalSum |> Answer.int64

let getPuzzles() = 
    "aoc2018/day12", [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 325)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 1917)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 1250000000991L)
    ]

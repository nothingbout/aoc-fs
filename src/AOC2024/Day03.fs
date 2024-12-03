module AOC2024.Day03
open Utils
open Utils.Globals

type Instruction = Mul of int * int | Do | Dont

let scanMul = 
    Scan.scan {
        do! Scan.skipString "mul("
        let! x = Scan.takePositiveInt
        do! Scan.skipString ","
        let! y = Scan.takePositiveInt
        do! Scan.skipString ")"
        if x < 1000 && y < 1000 then return Mul (x, y)
        else return! Scan.error "more than three digits"
    }

let scanInstruction = 
    Scan.scan {
        match! scanMul >> Scan.maybe with 
        | Some mul -> return mul
        | None ->
        match! Scan.skipString "do()" >> Scan.maybe with
        | Some () -> return Do
        | None ->
        match! Scan.skipString "don't()" >> Scan.maybe with
        | Some () -> return Dont
        | None -> return! Scan.error "not an instruction"
    }

let rec scanInstructions (instructions : Instruction list) (substr : Substring) : Instruction list = 
    match substr |> Scan.next scanInstruction with
    | _, ScanError _ -> List.rev instructions
    | substr, ScanSuccess instruction -> substr |> scanInstructions (instruction :: instructions)

let solveP1 (inputLines: string list) = 
    let instructions = inputLines |> String.concat "" |> Substring.ofString |> scanInstructions []
    instructions 
    |> List.map (fun instruction ->
        match instruction with
        | Mul (x, y) -> x * y
        | _ -> 0
    ) 
    |> List.sum |> Answer.int

let solveP2 (inputLines: string list) = 
    let instructions = inputLines |> String.concat "" |> Substring.ofString |> scanInstructions []
    ((0, true), instructions) 
    ||> List.fold (fun (sum, enabled) instruction ->
        match instruction with
        | Mul (x, y) -> if enabled then (sum + x * y, enabled) else (sum, enabled)
        | Do -> (sum, true)
        | Dont -> (sum, false)
    ) 
    |> fst |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example1.txt" (Answer.int 161)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 178886550)
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.int 48)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 87163705)
    ]

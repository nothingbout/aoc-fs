module AOC2018.Day16
open Utils
open Utils.Globals

type Op =
    | Addr = 0
    | Addi = 1
    | Mulr = 2
    | Muli = 3
    | Banr = 4
    | Bani = 5
    | Borr = 6
    | Bori = 7
    | Setr = 8
    | Seti = 9
    | Gtir = 10
    | Gtri = 11
    | Gtrr = 12
    | Eqir = 13
    | Eqri = 14
    | Eqrr = 15

[<Struct>]
type Instruction = { Op : Op; Args : struct(int * int * int) }

module Instruction = 
    let make op args = { Op = op; Args = args }
    let ofList (arr : int list) = 
        match arr with
        | [op; a; b; c] -> make (enum<Op> op) struct(a, b, c)
        | _ -> failwith $"expected four integers but got {arr}"

[<Struct>]
type Registers = { R0 : int; R1 : int; R2 : int; R3 : int }

module Registers =
    let make r0 r1 r2 r3 = { R0 = r0; R1 = r1; R2 = r2; R3 = r3 }
    let ofList (arr : int list) = 
        match arr with
        | [r0; r1; r2; r3] -> make r0 r1 r2 r3
        | _ -> failwith $"expected four integers but got {arr}"

let scanInstruction line = 
    line |> String.extractInts |> Instruction.ofList

let rec scanSamples samples inputLines = 
    match inputLines with
    | l1 :: l2 :: l3 :: l4 :: inputLines when l1 |> String.startsWith "Before:" -> 
        let before = l1 |> String.extractInts |> Registers.ofList
        let instr = l2 |> scanInstruction
        let after = l3 |> String.extractInts |> Registers.ofList
        inputLines |> scanSamples ((before, instr, after) :: samples)
    | _ ->
        (samples |> List.rev, inputLines)

let getr idx state = 
    match idx with
    | 0 -> state.R0
    | 1 -> state.R1
    | 2 -> state.R2
    | 3 -> state.R3
    | _ -> failwith $"unexpected register {idx}"

let setr idx value state = 
    match idx with
    | 0 -> { state with R0 = value }
    | 1 -> { state with R1 = value }
    | 2 -> { state with R2 = value }
    | 3 -> { state with R3 = value }
    | _ -> failwith $"unexpected register {idx}"

let doopr fn struct(a, _, c) state =
    state |> setr c (fn (getr a state) 0)

let doopi fn struct(a, _, c) state =
    state |> setr c (fn a 0)

let dooprr fn struct(a, b, c) state =
    state |> setr c (fn (getr a state) (getr b state))

let doopri fn struct(a, b, c) state =
    state |> setr c (fn (getr a state) b)

let doopir fn struct(a, b, c) state =
    state |> setr c (fn a (getr b state))

let doop op args state =
    let (|>!) = fun a _ -> a
    let (>?) = fun a b -> if a > b then 1 else 0
    let (=?) = fun a b -> if a = b then 1 else 0
    match op with 
    | Op.Addr -> state |> dooprr (+) args
    | Op.Addi -> state |> doopri (+) args
    | Op.Mulr -> state |> dooprr ( * ) args
    | Op.Muli -> state |> doopri ( * ) args
    | Op.Banr -> state |> dooprr (&&&) args
    | Op.Bani -> state |> doopri (&&&) args
    | Op.Borr -> state |> dooprr (|||) args
    | Op.Bori -> state |> doopri (|||) args
    | Op.Setr -> state |> doopr (|>!) args
    | Op.Seti -> state |> doopi (|>!) args
    | Op.Gtir -> state |> doopir (>?) args
    | Op.Gtri -> state |> doopri (>?) args
    | Op.Gtrr -> state |> dooprr (>?) args
    | Op.Eqir -> state |> doopir (=?) args
    | Op.Eqri -> state |> doopri (=?) args
    | Op.Eqrr -> state |> dooprr (=?) args
    | _ -> failwith $"unexpected op {op}"

let allMappings () = 
    seq { 0..15 } |> Seq.map (fun i -> 
        (i |> enum<Op>, [0..15] |> List.map (enum<Op>))
    ) |> Map.ofSeq

let rec possibleOpMappingsFromSamples mappings samples =
    match samples with
    | [] -> mappings
    | (before, { Op = i; Args = args }, after) :: samples ->
        let nops = mappings |> Map.find i |> List.filter (fun j ->
            before |> doop j args = after
        )
        samples |> possibleOpMappingsFromSamples (mappings |> Map.add i nops)

let rec reducePossibleOpMappings mappings possibleMappings =
    match possibleMappings |> Map.tryFindKey (fun _ ops -> List.length ops = 1) with
    | None -> 
        if Map.isEmpty possibleMappings then mappings
        else failwith "couldn't find a mapping with only one option"
    | Some i -> 
        let j = possibleMappings |> Map.find i |> List.exactlyOne
        possibleMappings 
        |> Map.remove i 
        |> Map.map (fun _ ops -> ops |> List.except [j])
        |> reducePossibleOpMappings (mappings |> Map.add i j)

let solveP1 (inputLines: string list) = 
    let samples, _ = inputLines |> scanSamples []
    samples |> Seq.filter (fun (before, { Op = _; Args = args }, after) -> 
        seq { 0..15 } |> Seq.filter (fun op -> 
            before |> doop (enum<Op> op) args = after
        ) |> Seq.length >= 3
    ) |> Seq.length |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let samples, inputLines = inputLines |> scanSamples []
    let testProgram = inputLines |> List.skipWhile String.isEmpty |> List.map scanInstruction

    let mappings = 
        samples 
        |> possibleOpMappingsFromSamples (allMappings ())
        |> reducePossibleOpMappings Map.empty

    (Registers.make 0 0 0 0, testProgram) ||> List.fold (fun state { Op = i; Args = args } ->
        let j = mappings |> Map.find i
        state |> doop j args
    ) |> getr 0 |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 624)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 584)
    ]

module AOC2024.Day24
open Utils

[<RequireQualifiedAccess>]
type Op = AND | OR | XOR

[<Struct>]
type Gate = { Op: Op; In1: string; In2: string; Out1: string; Swapped: bool }

let parseRegister line = 
    match line |> String.splitByString ": " with 
    | [name; value] -> (name, int value)
    | _ -> failwith "unexpected"

let parseOp str = 
    match str with 
    | "AND" -> Op.AND
    | "OR" -> Op.OR
    | "XOR" -> Op.XOR
    | _ -> failwith "unexpected"

let parseGate line = 
    match line |>  String.splitByString " " with
    | [in1; op; in2; _; out1] -> { Op = parseOp op; In1 = in1; In2 = in2; Out1 = out1; Swapped = false }
    | _ -> failwith "unexpected"

let parseInput lines = 
    let registerLines, gateLines = lines |> List.splitAtFirst String.isEmpty
    registerLines |> List.map parseRegister, gateLines |> List.skip 1 |> List.map parseGate
    
let opname op = 
    match op with
    | Op.AND -> "AND"
    | Op.OR -> "OR"
    | Op.XOR -> "XOR"

let doop op in1 in2 =
    match op with 
    | Op.AND -> in1 &&& in2
    | Op.OR -> in1 ||| in2
    | Op.XOR -> in1 ^^^ in2

let rec computeRegister registers gatesOutMap reg = 
    match Map.tryFind reg registers with
    | Some x -> x
    | None ->
        let gate = Map.find reg gatesOutMap
        doop gate.Op (computeRegister registers gatesOutMap gate.In1) (computeRegister registers gatesOutMap gate.In2)

let registerName prefix idx = 
    if idx < 10 then $"{prefix}0{idx}" else $"{prefix}{idx}"

let computeDecimal registers gatesOutMap prefix = 
    let rec loop idx = 
        let name = registerName prefix idx
        if Map.containsKey name gatesOutMap || Map.containsKey name registers then
            ((loop (idx + 1)) <<< 1) + int64 (computeRegister registers gatesOutMap name)
        else
            0L
    loop 0

let rec computeMaxDepth gates out1 = 
    match Map.tryFind out1 gates with
    | Some gate -> 
        max (computeMaxDepth gates gate.In1) (computeMaxDepth gates gate.In2) + 1
    | None -> 0

let sortInputs gates gate = 
    let swap = 
        computeMaxDepth gates gate.In1 > computeMaxDepth gates gate.In2 ||
        (String.startsWith "y" gate.In1 && String.startsWith "x" gate.In2)
    if swap then { gate with In1 = gate.In2; In2 = gate.In1 }
    else gate

let swapGates gates (a : string) (b : string) = 
    let na = { Map.find a gates with Out1 = b; Swapped = true }
    let nb = { Map.find b gates with Out1 = a; Swapped = true }
    gates |> Map.add na.Out1 na |> Map.add nb.Out1 nb

let rec printGates gates seen depth reg =
    if Dict.containsKey seen reg then () else
    Dict.add seen reg true
    match Map.tryFind reg gates with
    | Some gate ->
        let indent = String.replicate depth "  "
        printfn $"{indent}{gate.Out1} <- {gate.In1} {opname gate.Op} {gate.In2}"
        printGates gates seen (depth + 1) gate.In1 
        printGates gates seen (depth + 1) gate.In2
    | None -> ()

let randomValidateGates gates = 
    let mutable registers = Map.empty
    seq { 0..44 } |> Seq.iter (fun idx ->
        let x = System.Random.Shared.Next () % 2
        let y = System.Random.Shared.Next() % 2
        registers <- Map.add (registerName "x" idx) x registers
        registers <- Map.add (registerName "y" idx) y registers
    )
    let x = computeDecimal registers gates "x"
    let y = computeDecimal registers gates "y"
    x + y = computeDecimal registers gates "z"

let solveP1 (inputLines: string list) = 
    let registers, gates = inputLines |> parseInput
    let registersMap = registers |> Map.ofSeq
    let gatesOutMap = gates |> List.map (fun gate -> gate.Out1, gate) |> Map.ofList
    computeDecimal registersMap gatesOutMap "z" |> Answer.int64

let solveP2 (inputLines: string list) = 
    let registers, gates = inputLines |> parseInput
    // let registers = registers |> Map.ofSeq

    let gates = gates |> List.map (fun gate -> gate.Out1, gate) |> Map.ofList

    let swaps = [
        ("gws", "nnt")
        ("z13", "npf")
        ("z19", "cph")
        ("z33", "hgj")
    ]
    let gates = (gates, swaps) ||> Seq.fold (fun gates (a, b) -> swapGates gates a b)

    let gates = gates |> Map.map (fun _ gate -> sortInputs gates gate)

    // let seen = Dict.makeEmpty ()
    // for idx = 2 to 44 do
    //     printGates gates seen 0 (registerName "z" idx)

    for idx = 2 to 44 do
        let root = Map.find (registerName "z" idx) gates
        if root.Op <> Op.XOR then failwith $"Expected XOR at {root}"

        let xor1 = Map.find root.In1 gates
        if xor1.Op <> Op.XOR then failwith $"Expected XOR at {xor1}"
        let xn = registerName "x" idx in 
            if xor1.In1 <> xn then failwith $"Expected {xn} at {xor1}"
        let yn = registerName "y" idx in 
            if xor1.In2 <> yn then failwith $"Expected {yn} at {xor1}"

        let or1 = Map.find root.In2 gates
        if or1.Op <> Op.OR then failwith $"Expected OR at {or1}"

        let and1 = Map.find or1.In1 gates
        if and1.Op <> Op.AND then failwith $"Expected AND at {and1}"
        let xn = registerName "x" (idx - 1) in 
            if and1.In1 <> xn then failwith $"Expected {xn} at {and1}"
        let yn = registerName "y" (idx - 1) in 
            if and1.In2 <> yn then failwith $"Expected {yn} at {and1}"

        let and2 = Map.find or1.In2 gates
        if and2.Op <> Op.AND then failwith $"Expected AND at {and2}"

    if not (seq { 0..100 } |> Seq.forall (fun _ -> randomValidateGates gates)) then 
        printfn $"VALIDATION FAILED"

    swaps |> List.map (fun (a, b) -> [a; b]) |> List.concat 
    |> List.sort |> String.concat "," |> Answer.string

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 2024)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 42049478636360L)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.string "cph,gws,hgj,nnt,npf,z13,z19,z33")
    ]

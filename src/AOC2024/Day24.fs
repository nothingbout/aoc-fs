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

let isLooped gates reg = 
    let rec loop visited reg = 
        if List.contains reg visited then true else
        let visited = reg :: visited
        match Map.tryFind reg gates with
        | Some gate -> loop visited gate.In1 || loop visited gate.In2
        | None -> false
    loop [] reg

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

let swapGates gates (a : Gate) (b : Gate) = 
    let na = { a with Out1 = b.Out1; Swapped = true }
    let nb = { b with Out1 = a.Out1; Swapped = true }
    gates |> Map.add na.Out1 na |> Map.add nb.Out1 nb

let rec printGates gates reg depth =
    match Map.tryFind reg gates with
    | Some gate ->
        let indent = String.replicate depth "  "
        printfn $"{indent}{gate.Out1} <- {gate.In1} {opname gate.Op} {gate.In2}"
        printGates gates gate.In1 (depth + 1)
        printGates gates gate.In2 (depth + 1)
    | None -> ()

let hasInputs gate a b = 
    if gate.In1 = a && gate.In2 = b then true else
    if gate.In2 = a && gate.In1 = b then true else
    false

let isRootGateForIdx gates idx gate = 
    let isXYXOR out1 = 
        match Map.tryFind out1 gates with
        | Some gate ->
            if gate.Op = Op.XOR && hasInputs gate (registerName "x" idx) (registerName "y" idx) then true
            else
                false
        | _ -> false

    if gate.Op = Op.XOR && (isXYXOR gate.In1 || isXYXOR gate.In2) then true
    else false

let randomValidateGates registers gates = 
    let mutable registers = registers
    seq { 0..44 } |> Seq.iter (fun idx ->
        let x = System.Random.Shared.Next () % 2
        let y = System.Random.Shared.Next() % 2
        registers <- Map.add (registerName "x" idx) x registers
        registers <- Map.add (registerName "y" idx) y registers
    )
    let x = computeDecimal registers gates "x"
    let y = computeDecimal registers gates "y"
    x + y = computeDecimal registers gates "z"

let validateGates registers expected gates = 
    if seq { 0..45 } |> Seq.exists (fun idx -> isLooped gates (registerName "z" idx)) then false else
    if expected <> computeDecimal registers gates "z" then false else
    seq { 0..50 } |> Seq.forall (fun _ -> randomValidateGates registers gates)

let solveP2 (inputLines: string list) = 
    let registers, gates = inputLines |> parseInput
    let registers = registers |> Map.ofSeq
    let mutable gates = gates |> List.map (fun gate -> gate.Out1, gate) |> Map.ofList

    let mutable swaps = []

    for idx = 2 to 44 do
        let gate1 = Map.find (registerName "z" idx) gates
        if not (isRootGateForIdx gates idx gate1) then
            match Map.values gates |> Seq.tryFind (isRootGateForIdx gates idx) with
            | Some gate2 ->
                gates <- swapGates gates gate1 gate2
                swaps <- (gate1.Out1, gate2.Out1) :: swaps
            | None ->
                // printfn $"Skipped {gate1.Out1}"
                ()

    let x = computeDecimal registers gates "x"
    let y = computeDecimal registers gates "y"

    let lastSwap = 
        Map.values gates |> Seq.map (fun gate1 ->
            if gate1.Swapped then Seq.empty else
            Map.values gates |> Seq.choose (fun gate2 -> 
                if gate2.Swapped then None else
                if gate1 < gate2 && validateGates registers (x + y) (swapGates gates gate1 gate2) then
                    Some (gate1.Out1, gate2.Out1)
                else None
            )
        ) 
        |> Seq.concat
        |> Seq.exactlyOne

    swaps <- lastSwap :: swaps

    swaps |> List.map (fun (a, b) -> [a; b]) |> List.concat |> List.sort
    |> String.concat "," |> Answer.string

let solveP1 (inputLines: string list) = 
    let registers, gates = inputLines |> parseInput
    let registersMap = registers |> Map.ofSeq
    let gatesOutMap = gates |> List.map (fun gate -> gate.Out1, gate) |> Map.ofList
    computeDecimal registersMap gatesOutMap "z" |> Answer.int64

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 2024)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 42049478636360L)
        // Puzzle.create solveP2 "Part 2" "input.txt" (Answer.string "cph,gws,hgj,nnt,npf,z13,z19,z33")
    ]

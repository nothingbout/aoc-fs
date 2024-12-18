module AOC2024.Day17
open Utils

type Op =
    | Adv = 0
    | Bxl = 1
    | Bst = 2
    | Jnz = 3
    | Bxc = 4
    | Out = 5
    | Bdv = 6
    | Cdv = 7

type State = { Registers : int64 * int64 * int64; Program : int array; Ptr : int; Output : int list; Halted : bool }

let defaultState = { Registers = (0, 0, 0); Program = [||]; Ptr = 0; Output = []; Halted = false }

let parseRegister line = 
    line |> String.extractInts |> List.map int64 |> List.head

let parseProgram lines = 
    match lines with
    | [al; bl; cl; ""; pl] ->
        let program = pl |> String.trimPrefix "Program: " |> String.splitByString "," |> List.map int |> Array.ofList
        { defaultState with Registers = (parseRegister al, parseRegister bl, parseRegister cl); Program = program }
    | _ -> failwith "unexpected"

let combo operand state = 
    let a, b, c = state.Registers
    match operand with
    | 0 | 1 | 2 | 3 -> int64 operand
    | 4 -> a
    | 5 -> b
    | 6 -> c
    | 7 -> failwith "reserved"
    | _ -> failwith $"unexpected operand {operand}"

let literal operand state = operand

let geta state = let a, _, _ = state.Registers in a
let getb state = let _, b, _ = state.Registers in b
let getc state = let _, _, c = state.Registers in c

let seta value state = 
    let _, b, c = state.Registers
    { state with Registers = (value, b, c) }

let setb value state = 
    let a, _, c = state.Registers
    { state with Registers = (a, value, c) }

let setc value state = 
    let a, b, _ = state.Registers
    { state with Registers = (a, b, value) }

let jump pos state = 
    { state with Ptr = pos }

let advance state = 
    state |> jump (state.Ptr + 2)

let doop op fa fb fo state =
    let r = op (fa state) (fb state)
    fo r state |> advance

let step state = 
    if state.Ptr > Array.length state.Program - 2 then { state with Halted = true } else
    let opcode = enum<Op> state.Program[state.Ptr]
    let operand = state.Program[state.Ptr + 1]
    match opcode with
    | Op.Adv -> state |> doop (fun a b -> a / (pown 2L (int b))) geta (combo operand) seta
    | Op.Bxl -> state |> doop (fun a b -> a ^^^ b) getb (literal operand) setb
    | Op.Bst -> state |> doop (fun a _ -> a %+ 8L) (combo operand) (combo operand) setb
    | Op.Jnz -> if geta state <> 0 then jump operand state else state |> advance
    | Op.Bxc -> state |> doop (fun a b -> a ^^^ b) getb getc setb
    | Op.Out -> { state with Output = (combo operand state %+ 8L |> int) :: state.Output } |> advance
    | Op.Bdv -> state |> doop (fun a b -> a / (pown 2L (int b))) geta (combo operand) setb
    | Op.Cdv -> state |> doop (fun a b -> a / (pown 2L (int b))) geta (combo operand) setc
    | _ -> failwith $"unexpected opcode {opcode}"

let stepUntilHalted state = 
    let mutable state = state
    while not state.Halted do
        state <- step state
    state

let rec search program outputs n a = 
    // (a, outputs) |> inspect |> ignore
    match outputs with 
    | [] -> Some a
    | nextOut :: outputs ->
        seq {0L..7L} |> Seq.tryPick (fun nextIn ->
            let a = a * 8L + nextIn
            let state = program |> seta a |> stepUntilHalted
            match state.Output |> List.tryItem n with
            | Some o when o = nextOut -> search program outputs (n + 1) a
            | _ -> None
        )
    
let solveP1 (inputLines: string list) = 
    let program = inputLines |> parseProgram
    let state = stepUntilHalted program
    state.Output |> List.rev |> List.map toString |> String.concat "," |> Answer.string
    
let solveP2 (inputLines: string list) = 
    let program = inputLines |> parseProgram
    let a = search program (List.ofArray program.Program |> List.rev) 0 0 |> Option.get
    Answer.int64 a

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.string "4,6,3,5,6,3,5,2,1,0")
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.string "3,6,7,0,5,7,3,1,4")
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.int64 117440)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int64 164278496489149L)
    ]

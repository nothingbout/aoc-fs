module AOC2018.Day14
open Utils
open Utils.Globals

[<Struct>]
type State = { Recipes : int array; MadeCount : int; I : int; J : int }

let makeFirstRecipe state = 
    let (recipes, madeCount) = 
        match state.Recipes[state.I] + state.Recipes[state.J] with
        | sum when sum >= 10 -> 
            (state.Recipes, state.MadeCount) |> ArrayList.mutAppend (sum / 10) 
        | sum -> 
            (state.Recipes, state.MadeCount) |> ArrayList.mutAppend (sum) 
    { state with Recipes = recipes; MadeCount = madeCount }

let makeSecondRecipe state = 
    let (recipes, madeCount) = 
        match state.Recipes[state.I] + state.Recipes[state.J] with
        | sum when sum >= 10 -> 
            (state.Recipes, state.MadeCount) |> ArrayList.mutAppend (sum % 10)
        | _ -> 
            (state.Recipes, state.MadeCount) 
    { state with Recipes = recipes; MadeCount = madeCount }

let advanceIndices state = 
    {state with 
        I = ((state.I + 1 + state.Recipes[state.I]) % state.MadeCount)
        J = ((state.J + 1 + state.Recipes[state.J]) % state.MadeCount) 
    }

let rec stepUntil endCond state = 
    if endCond state then state
    else
    let state = makeFirstRecipe state
    if endCond state then state
    else
    makeSecondRecipe state |> advanceIndices |> stepUntil endCond

let makeInitialState () = { Recipes = [|3; 7|]; MadeCount = 2; I = 0; J = 1 } 

let solveP1 (inputLines: string list) = 
    let practiceCount = inputLines |> List.head |> int
    let finalState = makeInitialState () |> stepUntil (fun state -> state.MadeCount = practiceCount + 10)
    seq { -10..-1 } |> Seq.map (fun i -> finalState.Recipes[finalState.MadeCount + i] |> toString) |> String.concat "" |> Answer.string
    
let solveP2 (inputLines: string list) = 
    let ns = inputLines |> List.head |> Array.ofSeq |> Array.map (fun c -> c |> toString |> int)
    let n = Array.length ns
    let finalState = makeInitialState () |> stepUntil (fun state -> 
        state.MadeCount >= n && seq { 1..n } |> Seq.forall (fun i -> state.Recipes[state.MadeCount - i] = ns[n - i] )
    )
    finalState.MadeCount - n |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example1.txt" (Answer.string "5158916779")
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.string "4910101614")
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.int 2018)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 20253137)
    ]

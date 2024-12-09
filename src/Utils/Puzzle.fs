namespace Utils

module Answer = 
    type T = 
    | None
    | Int of int
    | Int64 of int64
    | Bigint of bigint
    | String of string
    with
        override this.ToString() = 
            match this with
            | None -> $"None"
            | Int x -> $"{x}"
            | Int64 x -> $"int64 {x}"
            | Bigint x -> $"bigint {x}"
            | String x -> $"\"{x}\""

    let none = None
    let int x = Int x
    let int64 x = Int64 x
    let bigint x = Bigint x
    let string x = String x

type Puzzle = {
    Solve : string list -> Answer.T 
    Label : string
    Input : string
    Expected : Answer.T
    }
    
module Puzzle =
    type Result = 
    | Success
    | Fail
    | InputNotFound

    type Results = {
        Success : int
        Fail : int
        InputNotFound : int
    }

    let create solve label input expected = 
        {Solve = solve; Label = label; Input = input; Expected = expected}

    let run path puzzle = 
        let inputFile = $"data/{path}/{puzzle.Input}"
        match File.tryReadLinesWithLogging inputFile with
        | None -> Result.InputNotFound
        | Some inputLines ->
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let answer = puzzle.Solve inputLines
        sw.Stop()
        let result = if puzzle.Expected = Answer.None || answer = puzzle.Expected then Success else Fail
        let mainOutput = $"[{puzzle.Label}, {puzzle.Input}]: {answer}"
        let resultOutput = 
            match result with
            | Success -> " [OK]"
            | Fail -> $" != {puzzle.Expected} [FAIL]"
            | InputNotFound -> failwith "unexpected"
        let formattedTime = Printf.sprintf "%.2f ms" sw.Elapsed.TotalMilliseconds
        printfn $"{mainOutput}{resultOutput}, took {formattedTime}"
        result

    let runMany path puzzles (results : Results) : Results = 
        printfn $"{path}:"
        puzzles |> List.fold (fun results puzzle ->
            match run path puzzle with
            | Success -> {results with Success = results.Success + 1}
            | Fail -> {results with Fail = results.Fail + 1}
            | InputNotFound -> {results with InputNotFound = results.InputNotFound + 1}
        ) results
    
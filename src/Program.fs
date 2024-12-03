open Utils
open Utils.Globals

// https://stackoverflow.com/questions/21909506/can-i-call-a-function-by-name-in-f

// open System.Reflection

// let rec fsharpName (mi : MemberInfo) =
//     if mi.DeclaringType.IsNestedPublic then
//         sprintf "%s.%s" (fsharpName mi.DeclaringType) mi.Name
//     else
//         mi.Name

// let functionsByName = 
//       Assembly.GetExecutingAssembly().GetTypes()
//                 |> Seq.filter (fun t -> t.IsPublic || t.IsNestedPublic) 
//                 |> Seq.collect (fun t -> t.GetMethods(BindingFlags.Static ||| BindingFlags.Public))
//                 |> Seq.filter (fun m -> not m.IsSpecialName)
//                 |> Seq.groupBy (fun m -> fsharpName m)
//                 |> Map.ofSeq
//                 |> Map.map (fun k v -> Seq.exactlyOne v)

// functionsByName.[fsharpFunctionNameString].Invoke(null, objectArrayOfArguments)

let rootModulesInNamespace (ns : string) = 
    System.Reflection.Assembly.GetExecutingAssembly().GetTypes()
    |> Seq.filter (fun t -> t.IsPublic)
    |> Seq.filter (fun t -> 
        match t.FullName |> Str.tryTrimPrefix (ns + ".") with
        | Some name -> name |> Str.tryFindIndexOfString "." = None
        | None -> false
    )

let tryFindFunctionInModule (name : string) (t : System.Type) = 
     t.GetMethods(System.Reflection.BindingFlags.Static ||| System.Reflection.BindingFlags.Public)
     |> Seq.tryFind (fun m -> m.Name = name)

let callFunctionInModule (m : System.Reflection.MethodInfo) (t : System.Type) : 'a =
    if not (m.GetParameters() |> Array.isEmpty) then 
        failwith $"{t.FullName}.{m.Name} should not take any parameters"
    let result = m.Invoke(null, Array.empty)
    try result :?> 'a
    with | :? System.InvalidCastException  as ex -> 
        failwith $"{t.FullName}.{m.Name} has wrong return type: {ex.Message}"

let getPuzzlesByPathInNamespace (ns : string) : (string * Puzzle list) list = 
    rootModulesInNamespace ns
    |> Seq.collect (fun t -> 
        match t |> tryFindFunctionInModule "getPuzzles" with
        | Some m -> 
            let puzzles = callFunctionInModule m t 
            let path = t.FullName |> Str.toLower |> Str.replaceOccurencesOfString "." "/"
            (path, puzzles) |> Seq.singleton
        | None -> Seq.empty
    ) |> List.ofSeq

[<EntryPoint>]
let main args =
    let runPath = if args.Length > 0 then args[0] |> Str.toLower else ""

    let puzzlesByPath = 
        seq {
            getPuzzlesByPathInNamespace "Benchmark"
            getPuzzlesByPathInNamespace "AOC2018"
            getPuzzlesByPathInNamespace "AOC2024"
        } |> List.concat

    let results : Puzzle.Results = {Success = 0; Fail = 0}
    let results = 
        puzzlesByPath |> List.fold
            (fun (results: Puzzle.Results) (path: string, puzzles: Puzzle list) -> 
                if path.StartsWith runPath then
                    Puzzle.runMany path puzzles results else results) 
            results

    match results with
    | {Success = 0; Fail = 0} -> 
        let paths = puzzlesByPath |> List.map (fun (path, _) -> path) |> List.sort
        printfn "No puzzles found in '%s', try one of these:\n%s" runPath (paths |> String.concat "\n")
    | results -> 
        printfn $"Success: {results.Success}, Fail: {results.Fail}"
    0

    
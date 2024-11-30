open Utils

[<EntryPoint>]
let main args =
    let runPath = if args.Length > 0 then args[0] else ""
    let puzzlesByPath = [
        Benchmark.Vec2Benchmark.getPuzzles ()
        AOC2018.Day01.getPuzzles ()
        AOC2018.Day02.getPuzzles ()
        AOC2018.Day03.getPuzzles ()
        AOC2018.Day04.getPuzzles ()
        AOC2018.Day05.getPuzzles ()
        AOC2018.Day06.getPuzzles ()
        AOC2018.Day07.getPuzzles ()
        AOC2018.Day08.getPuzzles ()
        AOC2018.Day09.getPuzzles ()
        AOC2018.Day10.getPuzzles ()
        AOC2018.Day11.getPuzzles ()
        AOC2018.Day12.getPuzzles ()
        AOC2018.Day13.getPuzzles ()
        AOC2018.Day14.getPuzzles ()
        AOC2018.Day15.getPuzzles ()
    ]

    let results : Puzzle.Results = {Success = 0; Fail = 0}
    let results = 
        puzzlesByPath |> List.fold
            (fun (results: Puzzle.Results) (path: string, puzzles: Puzzle list) -> 
                if path.StartsWith runPath then
                    Puzzle.runMany path puzzles results else results) 
            results

    match results with
    | {Success = 0; Fail = 0} -> printfn $"No puzzles found in '{runPath}'"
    | results -> printfn $"Success: {results.Success}, Fail: {results.Fail}"
    0

    
module AOC2024.Day06
open Utils
open Utils.Globals

let parseMap lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                if c = '#' then yield Vec2.make x y
    } |> Set.ofSeq

let parseGuard lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                if c = '^' then yield (Vec2.make x y, c)
    } |> Seq.exactlyOne

let allDirs = ['>'; '^'; '<'; 'v']

let dirToVec dir =
    match dir with
    | '>' -> Vec2.make 1 0
    | '^' -> Vec2.make 0 -1
    | '<' -> Vec2.make -1 0
    | 'v' -> Vec2.make 0 1
    | _ -> failwith "unexpected"

let turnRight dir =
    match dir with 
    | '>' -> 'v'
    | '^' -> '>'
    | '<' -> '^'
    | 'v' -> '<'
    | _ -> failwith "unexpected"

type State = { Pos : int Vec2; Dir : char; Visited : (int Vec2 * char) Set }

module State = 
    let make pos dir visited = {Pos = pos; Dir = dir; Visited = visited}
    let hasVisitedPos pos state = allDirs |> Seq.exists (fun dir -> state.Visited |> Set.contains (pos, dir))
    let isOOB bounds state = Rect.contains state.Pos bounds |> not

let moveOnce map state =
    let mutable nextDir = state.Dir
    while map |> Set.contains (state.Pos + dirToVec nextDir) do
        nextDir <- turnRight nextDir
    State.make (state.Pos + dirToVec nextDir) nextDir (state.Visited |> Set.add (state.Pos, state.Dir))

let getAllStatesUntilOOB map bounds state =
    state |> Seq.unfold (fun state -> 
        if State.isOOB bounds state then None
        else Some (state, moveOnce map state)
    )

let rec tryFindCycle map traversalGrid state = 
    match TraveralGrid.tryFindInDirection state.Pos (dirToVec state.Dir) traversalGrid with
    | None -> false
    | Some obstaclePos -> 
        let nextPos = obstaclePos - (dirToVec state.Dir)
        if state.Visited |> Set.contains (nextPos, state.Dir) then true
        else {state with Pos = nextPos} |> moveOnce map |> tryFindCycle map traversalGrid

let solveP1 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let guardPos, guardDir = inputLines |> parseGuard
    let bounds = Rect.encapsulating map

    State.make guardPos guardDir Set.empty |> getAllStatesUntilOOB map bounds
    |> Seq.map (fun state -> state.Pos) |> Set.ofSeq |> Set.count |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let guardPos, guardDir = inputLines |> parseGuard
    let bounds = Rect.encapsulating map
    let traversalGrid = TraveralGrid.make (Rect.expand (Vec2.make 1 1) bounds) map

    let states = State.make guardPos guardDir Set.empty |> getAllStatesUntilOOB map bounds

    let chunks = states |> Array.ofSeq |> Array.indexed |> Array.groupByAndMap (fun (i, _) -> i % System.Environment.ProcessorCount) (fun (_, s) -> s)
    chunks |> Array.Parallel.map (fun (chunkIdx, chunkStates) -> 
        // let sw = System.Diagnostics.Stopwatch.StartNew()
        let insertedObstacles = chunkStates |> Array.choose (fun state -> 
            let nextState = moveOnce map state
            if State.hasVisitedPos nextState.Pos state then None // Can't insert an obstacle here since we have already traveled through the position
            else
            // Insert an obstacle in nextState.Pos and check if it results in a cycle
            let isCycle = state |> tryFindCycle (Set.add nextState.Pos map) (TraveralGrid.addPosition nextState.Pos traversalGrid)
            if isCycle then Some nextState.Pos else None
        )
        // sw.Stop()
        // printfn $"task {chunkIdx} completed in {sw.Elapsed.TotalMilliseconds} ms"
        insertedObstacles
    ) |> Array.concat
    |> Set.ofSeq |> Set.remove guardPos |> Set.count |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 41)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 5199)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 6)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 1915)
    ]

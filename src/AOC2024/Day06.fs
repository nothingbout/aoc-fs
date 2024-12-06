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
    let isOOB bounds state = bounds |> Rect.contains state.Pos |> not

let moveOnce map state =
    let mutable nextDir = state.Dir
    while map |> Set.contains (state.Pos + dirToVec nextDir) do
        nextDir <- turnRight nextDir
    State.make (state.Pos + dirToVec nextDir) nextDir (state.Visited |> Set.add (state.Pos, state.Dir))

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

    State.make guardPos guardDir Set.empty |> Seq.unfold (fun state -> 
        if State.isOOB bounds state then None
        else Some (state.Pos, moveOnce map state)
    ) |> Set.ofSeq |> Set.count |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let guardPos, guardDir = inputLines |> parseGuard
    let bounds = Rect.encapsulating map
    let traversalGrid = TraveralGrid.make (Rect.expand (Vec2.make 1 1) bounds) map
    State.make guardPos guardDir Set.empty |> Seq.unfold (fun state -> 
        if State.isOOB bounds state then None
        else 
        let nextState = moveOnce map state
        if not (State.hasVisitedPos nextState.Pos state) then 
            // Insert an obstacle in nextState.Pos and check if it results in a cycle
            let isCycle = state |> tryFindCycle (Set.add nextState.Pos map) (TraveralGrid.addPosition nextState.Pos traversalGrid)
            if isCycle then Some (Some nextState.Pos, nextState)
            else Some (None, nextState)
        else
            // Can't insert an obstacle here since we have already traveled through the position
            Some (None, nextState)
    ) 
    |> Seq.choose id |> Set.ofSeq 
    |> Set.remove guardPos |> Set.count |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 41)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 5199)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 6)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 1915)
    ]

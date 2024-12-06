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

let moveForwards pos dir =
    match dir with
    | '>' -> pos + Vec2.make 1 0
    | '^' -> pos + Vec2.make 0 -1
    | '<' -> pos + Vec2.make -1 0
    | 'v' -> pos + Vec2.make 0 1
    | _ -> failwith "unexpected"

let turnRight dir =
    match dir with 
    | '>' -> 'v'
    | '^' -> '>'
    | '<' -> '^'
    | 'v' -> '<'
    | _ -> failwith "unexpected"

let rec turnRightUntilNoObstacle map pos dir = 
    if map |> Set.contains (moveForwards pos dir) 
    then turnRight dir |> turnRightUntilNoObstacle map pos 
    else dir

[<Struct>]
type State = { Pos : int Vec2; Dir : char; Visited : (int Vec2 * char) Set }

module State = 
    let make pos dir visited = {Pos = pos; Dir = dir; Visited = visited}
    let isInVisitedPos state = state.Visited |> Set.contains (state.Pos, state.Dir)

let moveOnce map state =
    let nextDir = state.Dir |> turnRightUntilNoObstacle map state.Pos
    let nextPos = moveForwards state.Pos nextDir
    State.make nextPos nextDir (state.Visited |> Set.add (state.Pos, state.Dir))

let rec stepUntilCycleOrOOB map bounds state = 
    if not (bounds |> Rect.contains state.Pos) then false, state
    else if State.isInVisitedPos state then true, state
    else moveOnce map state |> stepUntilCycleOrOOB map bounds

let solveP1 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let guardPos, guardDir = inputLines |> parseGuard
    let bounds = Rect.encapsulating map
    let _, state = State.make guardPos guardDir Set.empty |> stepUntilCycleOrOOB map bounds
    state.Visited |> Set.map fst |> Set.count |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap
    let guardPos, guardDir = inputLines |> parseGuard
    let bounds = Rect.encapsulating map
    let _, state = State.make guardPos guardDir Set.empty |> stepUntilCycleOrOOB map bounds
    state.Visited |> Set.map fst |> Seq.filter (fun pos -> 
        if pos = guardPos then false
        else
        let isCycle, _ = State.make guardPos guardDir Set.empty |> stepUntilCycleOrOOB (map |> Set.add pos) bounds
        isCycle
    ) |> Set.ofSeq |> Seq.length |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 41)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 5199)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 6)
        // Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 1915)
    ]

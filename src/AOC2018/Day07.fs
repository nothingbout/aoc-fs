module AOC2018.Day07
open Utils
open Utils.Globals

[<Struct>]
type Dependency = { Src : char; Dst : char }

let parseDependency line = 
    line |> ScanSeq.ofString |> Scan.scan {
        // Step D must be finished before step X can begin.
        do! Scan.skipString "Step "
        let! src = Scan.take 1
        do! Scan.skipString " must be finished before step "
        let! dst = Scan.take 1
        do! Scan.skipString " can begin."
        return { Src = src[0]; Dst = dst[0] }
    } |> Scan.finish


[<Struct>]
type Task = { Name : char; Mask : int; DepsMask : int }

let taskNameMask name = 1 <<< int (name - 'A')

let makeTasks deps = 
    seq {
        let names = deps |> List.map (fun dep -> [dep.Src; dep.Dst]) |> List.concat |> List.distinct
        for name in names do
            let depsMask = deps |> List.filter (fun dep -> dep.Dst = name) |> List.fold (fun mask dep -> mask ||| taskNameMask dep.Src) 0
            yield { Name = name; Mask = taskNameMask name; DepsMask = depsMask }
    } |> List.ofSeq

let availableTasks completedMask tasks = 
    tasks |> List.filter (fun task -> task.DepsMask &&& completedMask = task.DepsMask)

let rec findTaskOrder orderRev completedMask remaining = 
    if remaining = [] then List.rev orderRev
    else
    let available = availableTasks completedMask remaining
    let next = available |> List.sortBy (fun task -> task.Name) |> List.head
    findTaskOrder (next.Name :: orderRev) (completedMask ||| next.Mask) (List.except [next] remaining)

[<Struct>]
type StartedTask = { Task : Task; StartTime : int }

type FindCompletionTimeState = {
    TimeConst : int
    Workers : int
    Time : int
    Started : StartedTask list
    CompletedMask : int
    Remaining : Task list
}

let taskDuration timeConst name = timeConst + 1 + int (name - 'A')

let tickTime state = 
    let time = state.Time + 1
    let completed = state.Started |> List.filter (fun t -> t.StartTime + taskDuration state.TimeConst t.Task.Name <= time)
    { state with 
        Workers = state.Workers + List.length completed
        Time = time
        CompletedMask = (state.CompletedMask, completed) ||> List.fold (fun mask t -> mask ||| t.Task.Mask)
        Started = state.Started |> List.except completed
    }

let startTask task state = 
    { state with
        Workers = state.Workers - 1
        Started = { Task = task; StartTime = state.Time } :: state.Started
        Remaining = state.Remaining |> List.except [task]
    }

let rec findCompletionTime state = 
    if state.Started = [] && state.Remaining = [] then state.Time
    else
    match state.Workers, availableTasks state.CompletedMask state.Remaining with
    | workers, task :: _ when workers > 0 -> findCompletionTime (startTask task state)
    | _ -> findCompletionTime (tickTime state)

let solveP1 (inputLines: string list) = 
    let deps = inputLines |> List.map parseDependency
    let order = makeTasks deps |> findTaskOrder [] 0
    order |> Str.ofSeq |> Answer.string
    
let solveP2 timeConst workers (inputLines: string list) = 
    let deps = inputLines |> List.map parseDependency
    let tasks = makeTasks deps
    let time = findCompletionTime { TimeConst = timeConst; Workers = workers; Time = 0; Started = []; CompletedMask = 0; Remaining = tasks }
    Answer.int time

let getPuzzles() = 
    "aoc2018/day07", [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.string "CABDFE")
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.string "ADEFKLBVJQWUXCNGORTMYSIHPZ")
        Puzzle.create (solveP2 0 2) "Part 2" "example.txt" (Answer.int 15)
        Puzzle.create (solveP2 60 5) "Part 2" "input.txt" (Answer.int 1120)
    ]

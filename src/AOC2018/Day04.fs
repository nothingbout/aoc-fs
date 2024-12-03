module AOC2018.Day04
open Utils
open Utils.Globals

type DateTime = { Month : int; Day : int; Hour : int; Minute : int }

type Event = 
    | BeginsShift of int
    | FallsAsleep
    | WakesUp

type LogEntry = { Time : DateTime; Event : Event }

let parseLogEntry (line : string) : LogEntry = 
    line |> Substring.ofString |> Scan.scan {
        // [1518-11-10 23:46] Guard #1973 begins shift
        // [1518-08-14 00:52] falls asleep
        // [1518-05-17 00:55] wakes up
        do! Scan.skipString "[1518-"
        let! month = Scan.takePositiveInt
        do! Scan.skipString "-"
        let! day = Scan.takePositiveInt
        do! Scan.skipString " "
        let! hour = Scan.takePositiveInt
        do! Scan.skipString ":"
        let! minute = Scan.takePositiveInt
        do! Scan.skipString "] "

        let! evt = Scan.scan {
            match! Scan.skipString "Guard #" >> Scan.maybe with
            | Some () -> 
                let! id = Scan.takePositiveInt
                do! Scan.skipString " begins shift"
                return (BeginsShift id)
            | None -> 
            match! Scan.skipString "falls asleep" >> Scan.maybe with
            | Some () -> 
                return FallsAsleep
            | None ->
                do! Scan.skipString "wakes up"
                return WakesUp
        }

        let time = { Month = month; Day = day; Hour = hour; Minute = minute }
        return { Time = time; Event = evt }
    } |> Scan.finish

let sleepPeriodsByGuard entries : Map<int, list<Range<int>>> = 
    let rec _process entries curGuard sleepStart sleepPeriods = 
        match entries with
        | [] -> sleepPeriods |> Map.map (fun _ periods -> List.rev periods)
        | entry :: entries ->
            match entry.Event, sleepStart with
            | BeginsShift _, Some _ -> failwith "BeginsShift but has pending sleepStart"
            | BeginsShift guard, None -> _process entries guard None sleepPeriods
            | WakesUp, None -> failwith "WakesUp but doesn't have a pending sleepStart"
            | WakesUp, Some sleepStart ->
                let period = Range.make sleepStart.Minute (entry.Time.Minute - 1)
                let sleepPeriods = sleepPeriods |> Map.change curGuard (function
                    | None -> Some [period]
                    | Some periods -> Some (period :: periods))
                _process entries curGuard None sleepPeriods
            | FallsAsleep, Some _ -> failwith "FallsAsleep but already has a pending sleepStart"
            | FallsAsleep, None -> _process entries curGuard (Some entry.Time) sleepPeriods
    _process entries -1 None Map.empty

let findMostCommonMinute periods =
    let counts = System.Collections.Generic.Dictionary<_, _>()
    for period in periods do
        for i = period.Start to period.Finish do
            counts[i] <- 
                match counts.TryGetValue i with
                | true, value -> value + 1
                | _ -> 1
    counts :> seq<_> |> Seq.map (|KeyValue|) |> Seq.maxBy snd

let solveP1 (inputLines: string list) = 
    let entries = inputLines |> List.map parseLogEntry |> List.sortBy (fun entry -> entry.Time)
    entries |> sleepPeriodsByGuard |> Map.toSeq
    |> Seq.maxBy (fun (_, periods) -> periods |> List.sumBy (fun r -> r.Finish - r.Start + 1))
    |> fun (guard, periods) ->
        let (minute, _) = findMostCommonMinute periods
        Answer.int (guard * minute)

let solveP2 (inputLines: string list) = 
    let entries = inputLines |> List.map parseLogEntry |> List.sortBy (fun entry -> entry.Time)
    entries 
    |> sleepPeriodsByGuard |> Map.toSeq
    |> Seq.map (fun (guard, periods) -> (guard, findMostCommonMinute periods))
    |> Seq.maxBy (fun (_, (_, count)) -> count)
    |> fun (guard, (minute, _)) -> Answer.int (guard * minute)

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 12169)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 16164)
    ]




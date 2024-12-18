module Readme
open Utils

type Time = { Hours : int; Minutes : int; Seconds : int }
    with
        override this.ToString (): string = $"%02d{this.Hours}:%02d{this.Minutes}:%02d{this.Seconds}"

module Time =
    let totalMinutesRoundedUp time = 
        time.Hours * 60 + time.Minutes + (if time.Seconds > 0 then 1 else 0)

type Entry = { Rank : int; Time : Time }

let scanTime = Scan.scan {
    let! hours = Scan.takePositiveInt
    do! Scan.skipString ":"
    let! minutes = Scan.takePositiveInt
    do! Scan.skipString ":"
    let! seconds = Scan.takePositiveInt
    return { Hours = hours; Minutes = minutes; Seconds = seconds }
}

let parseGlobal lines = 
    let parseEntry line = 
        line |> Substr.ofStr |> Scan.scan {
            // 100) Dec 01  00:01:24
            let! rank = Scan.takePositiveInt
            do! Scan.skipString ") Dec "
            let! day = Scan.takePositiveInt
            do! Scan.skipSpaces
            let! time = scanTime
            return day, { Rank = rank; Time = time }
        } |> Scan.finish

    lines |> List.chunkBySize 2 |> List.mapi (fun idx lines ->
        match lines with
        | [l1; l2] -> 
            let day = idx + 1
            let d1, p1 = parseEntry l1
            if d1 <> day then failwith $"Expected day {day} but got {l1}"
            let d2, p2 = parseEntry l2
            if d2 <> day then failwith $"Expected day {day} but got {l2}"
            (day, p1, p2)
        | [_] -> failwith "global.txt should have an even number of lines"
        | _ -> failwith "unexpected")

let parsePersonal lines = 
    let headersLine = List.item 1 lines
    let entryLines = List.skip 2 lines
    let headers = headersLine |> String.extractGroupsBy (Char.isWhitespace >> not)
    let expectedHeaders = ["Day"; "Time"; "Rank"; "Score"; "Time"; "Rank"; "Score"]
    if headers <> expectedHeaders then 
        failwith (sprintf "Expected headers [%s] but got [%s]" (String.concat "; " expectedHeaders) (String.concat "; " headers))
    else
    let scanEntry = Scan.scan {
        let! time = scanTime
        do! Scan.skipSpaces
        let! rank = Scan.takePositiveInt
        do! Scan.skipSpaces
        let! _score = Scan.takePositiveInt
        return { Rank = rank; Time = time }
    }
    let parseDay line = 
        line |> Substr.ofStr |> Scan.scan {
            //   7   00:07:00   807      0   00:09:25   656      0
            do! Scan.skipSpaces
            let! day = Scan.takePositiveInt
            do! Scan.skipSpaces
            let! p1 = scanEntry
            do! Scan.skipSpaces
            let! p2 = scanEntry
            return day, p1, p2
        } |> Scan.finish
    entryLines |> List.rev |> List.mapi (fun idx line ->
        let day, p1, p2 = parseDay line
        if day <> idx + 1 then failwith $"Expected day {idx + 1} but got {day}"
        (day, p1, p2)
    )

let padBothSides len chr str = 
    let n = (len - String.length str) / 2
    let pad = Array.init n (fun _ -> chr) |> String.ofArray
    pad + str + pad + (if 2 * n + String.length str < len then chr |> toString else "")

let makeEntryLines timeVisCols day p1 p2 = 
    // Day 05  00:01:18   100  00:03:34   100  ----=                        
    //         00:05:29   890  00:10:15   560  --------------------------------------=======================
    //                                         =========
    let mins1 = Time.totalMinutesRoundedUp p1.Time
    let mins2 = Time.totalMinutesRoundedUp p2.Time - mins1
    let timeVisLines = 
        [Array.init mins1 (fun _ -> '*'); Array.init mins2 (fun _ -> '#')] |> Array.concat
        |> Array.chunkBySize timeVisCols |> Array.map String.ofArray |> List.ofArray

    let p1rank = $"#{p1.Rank}"
    let p2rank = $"#{p2.Rank}"
    let cols = $"{day,5}  {p1.Time} {p1rank,7}  {p2.Time} {p2rank,7}  "
    let extraLines = List.tail timeVisLines |> List.map (fun str -> String.init (String.length cols) (fun _ -> " ") + str)
    (cols + List.head timeVisLines) :: extraLines

let makeTimestableLines name path = 
    let dirPath = $"data/{path}/leaderboard"
    let globalEntries = File.tryReadLinesWithLogging $"{dirPath}/global.txt" |> Option.get |> parseGlobal
    let personalEntries = File.tryReadLinesWithLogging $"{dirPath}/personal.txt" |> Option.get |> parsePersonal
    if List.length globalEntries <> List.length personalEntries then failwith "global.txt and personal.txt should both have the same number of days"
    let timeVisCols = 60
    let timeVisHeader = padBothSides timeVisCols '-' "Minutes for Part 1 (*) + Part 2 (#)"
    let timeVisSpacer = String.init timeVisCols (fun _ -> "-")
    let mutable lines = [
        $"{name} global leaderboard rank #100 vs. personal times and ranks."
        $""
        $"-Day-  -----Part 1-----  ----Part 1+2----  {timeVisHeader}"
    ]
    for (gday, gp1, gp2), (pday, pp1, pp2) in List.zip globalEntries personalEntries do
        if gday <> pday then failwith $"Expected the days to be equal but got {gday} and {pday}"
        lines <- makeEntryLines timeVisCols $"%02d{gday}" gp1 gp2 |> List.append lines
        lines <- makeEntryLines timeVisCols "" pp1 pp2 |> List.append lines
        lines <- [$"-----  ----------------  ----------------  {timeVisSpacer}"] |> List.append lines
    // lines <- lines |> List.removeAt (List.length lines - 1)
    lines

let makeReadmeFile () = 
    let mutable lines = []
    lines <- makeTimestableLines "Advent of Code 2024" "aoc2024" |> List.append lines
    System.IO.File.WriteAllLines("src/AOC2024/README", lines |> Array.ofList)
    ()

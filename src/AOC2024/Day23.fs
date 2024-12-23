module AOC2024.Day23
open Utils

let parseConnection line = 
    match line |> String.splitByString "-" with
    | [a; b] -> (a, b)
    | _ -> failwith "unexpected"

let makeConnectionsMap connections = 
    let map1 = connections |> List.groupByAndMap fst snd |> Map.ofSeq
    let map2 = connections |> List.groupByAndMap snd fst |> Map.ofSeq
    Map.merge (@) map1 map2

let rec findLargestSet (connectionsMap : Map<_, _>) curSet curId = 
    match connectionsMap[curId] |> List.choose (fun nextId ->
        if nextId <= curId then None else
        if curSet |> Seq.forall (fun id -> connectionsMap[nextId] |> List.contains id) then
            Some (findLargestSet connectionsMap (curId :: curSet) nextId)
        else
            None
    ) with
    | [] -> List.rev (curId :: curSet)
    | sets -> sets |> List.maxBy List.length

let solveP1 (inputLines: string list) = 
    let connections = inputLines |> List.map parseConnection
    let connectionsMap = makeConnectionsMap connections

    let mutable sets = Set.empty
    for id1 in Map.keys connectionsMap do
        for id2 in connectionsMap[id1] do
            for id3 in connectionsMap[id1] do
                if connectionsMap[id2] |> List.contains id3 then
                    let ids = [id1; id2; id3]
                    if Seq.exists (String.startsWith "t") ids then
                        match [id1; id2; id3] |> List.sort with
                        | [a; b; c] -> sets <- Set.add (a, b, c) sets
                        | _ -> failwith "unexpected"

    Answer.int (Set.count sets)
    
let solveP2 (inputLines: string list) = 
    let connections = inputLines |> List.map parseConnection
    let connectionsMap = makeConnectionsMap connections

    Map.keys connectionsMap 
    |> Seq.map (findLargestSet connectionsMap [])
    |> Seq.maxBy (List.length) |> String.concat "," |> Answer.string

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 7)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 1149)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.string "co,de,ka,ta")
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.String "as,co,do,kh,km,mc,np,nt,un,uq,wc,wz,yo")
    ]

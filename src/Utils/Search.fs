module Utils.Search
open Utils
open Globals

[<Struct>]
type PathNode<'a, 'b> = { FromPos : 'a list; Distance : 'b; IsExpanded : bool }

module PathNode = 
    let fromPos node = node.FromPos
    let distance node = node.Distance
    let isExpanded node = node.IsExpanded

let getFirstPath toPos foundNodes = 
    [toPos]
    |> Seq.unfold (fun fromPos -> 
        match fromPos with
        | pos :: _ -> Some (foundNodes |> Map.find pos |> fun node -> (pos, node.FromPos))
        | [] -> None
    ) |> List.ofSeq |> List.rev

let rec private _allFromPositions endPos found nodes = 
    let nodes = Set.add endPos nodes
    match Map.tryFind endPos found with
    | None -> nodes
    | Some node ->
        (nodes, PathNode.fromPos node) 
        ||> Seq.fold (fun nodes pos -> _allFromPositions pos found nodes)

let rec allFromPositions endPos found = 
    _allFromPositions endPos found Set.empty

[<Struct>]
type ExpandResult<'p, 'd> =
| StopSearch
| Neighbors of ('p * 'd) seq

let private makeFoundMap found = 
    found |> Seq.map (|KeyValue|) |> Map.ofSeq
    |> Map.map (fun _ node -> {node with FromPos = List.rev node.FromPos})

///Guarantees first-in-first-out semantics for positions of equal distance.
let bfs (expand : 'p -> int -> ExpandResult<'p, int>) (fromPositions : 'p seq) : Map<'p, PathNode<'p, int>> =
        let mutable halt = false
        let queue = System.Collections.Generic.Queue<_>()
        let found = System.Collections.Generic.Dictionary<_, _>()
        for pos in fromPositions do
            queue.Enqueue(pos)
            found.Add(pos, { FromPos = []; Distance = 0; IsExpanded = false })
        while not halt && queue.Count > 0 do
            let pos = queue.Dequeue()
            let node = found[pos]
            if node.IsExpanded then failwith "unexpected node.IsExpanded = true"
            found[pos] <- { node with IsExpanded = true }
            match expand pos node.Distance with
            | Neighbors ns -> 
                for npos, ndist in ns do
                if ndist <> node.Distance + 1 then failwith "bfs expand should always return +1 distance"

                match found.TryGetValue(npos) with
                    | false, _ ->
                        queue.Enqueue(npos)
                        found.Add(npos, { FromPos = [pos]; Distance = ndist; IsExpanded = false })
                    | true, otherNode -> 
                        if ndist = otherNode.Distance then
                            found[npos] <- {otherNode with FromPos = pos :: otherNode.FromPos }
            | StopSearch ->
                halt <- true
        makeFoundMap found

///Does not guarantee first-in-first-out semantics for positions of equal distance.
let dijkstra (expand : 'p -> 'd -> ExpandResult<'p, 'd>) (fromPositions : ('p * 'd) seq) : Map<'p, PathNode<'p, 'd>> =
    let mutable halt = false
    let queue = System.Collections.Generic.PriorityQueue<_, _>()
    let found = System.Collections.Generic.Dictionary<_, _>()
    for pos, dist in fromPositions do
        queue.Enqueue(pos, dist)
        found.Add(pos, { FromPos = []; Distance = dist; IsExpanded = false })
    while not halt && queue.Count > 0 do
        let pos = queue.Dequeue()
        let node = found[pos]
        if not node.IsExpanded then
            found[pos] <- { node with IsExpanded = true }
            match expand pos node.Distance with
            | Neighbors ns -> 
                for npos, ndist in ns do
                    let isNewBest =
                        match found.TryGetValue(npos) with
                        | false, _ -> true
                        | true, otherNode -> 
                            if ndist < otherNode.Distance then true else
                            if ndist = otherNode.Distance then
                                found[npos] <- {otherNode with FromPos = pos :: otherNode.FromPos }
                            false
                    if isNewBest then
                        queue.Enqueue(npos, ndist)
                        found[npos] <- { FromPos = [pos]; Distance = ndist; IsExpanded = false}
            | StopSearch ->
                halt <- true
    makeFoundMap found

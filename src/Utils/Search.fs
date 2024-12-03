module Utils.Search
open Utils
open Utils.Globals

[<Struct>]
type PathNode<'a, 'b> = { FromPos : 'a option; Distance : 'b; IsExpanded : bool }

module PathNode = 
    let fromPos node = node.FromPos
    let distance node = node.Distance
    let isExpanded node = node.IsExpanded

let getPath toPos foundNodes = 
    Some toPos 
    |> Seq.unfold (fun posMaybe -> 
        match posMaybe with
        | Some pos -> Some (foundNodes |> Map.find pos |> fun node -> (pos, node.FromPos))
        | None -> None
    ) |> List.ofSeq |> List.rev

[<Struct>]
type ExpandResult<'p, 'd> =
| StopSearch
| Neighbors of ('p * 'd) seq

///Guarantees first-in-first-out semantics for positions of equal distance.
let bfs (expand : 'p -> int -> ExpandResult<'p, int>) (fromPositions : 'p seq) : Map<'p, PathNode<'p, int>> =
        let mutable halt = false
        let queue = System.Collections.Generic.Queue<_>()
        let found = System.Collections.Generic.Dictionary<_, _>()
        for pos in fromPositions do
            queue.Enqueue(pos)
            found.Add(pos, { FromPos = None; Distance = 0; IsExpanded = false })
        while not halt && queue.Count > 0 do
            let pos = queue.Dequeue()
            let node = found[pos]
            if node.IsExpanded then failwith "unexpected node.IsExpanded = true"
            found[pos] <- { node with IsExpanded = true }
            match expand pos node.Distance with
            | Neighbors ns -> 
                for npos, ndist in ns do
                if ndist <> node.Distance + 1 then failwith "bfs expand should always return +1 distance"
                if not (found.ContainsKey(npos)) then
                    queue.Enqueue(npos)
                    found.Add(npos, { FromPos = Some pos; Distance = ndist; IsExpanded = false })
            | StopSearch ->
                halt <- true
        found |> Seq.map (|KeyValue|) |> Map.ofSeq

///Does not guarantee first-in-first-out semantics for positions of equal distance.
let dijkstra (expand : 'p -> 'd -> ExpandResult<'p, 'd>) (fromPositions : ('p * 'd) seq) : Map<'p, PathNode<'p, 'd>> =
    let mutable halt = false
    let queue = System.Collections.Generic.PriorityQueue<_, _>()
    let found = System.Collections.Generic.Dictionary<_, _>()
    for pos, dist in fromPositions do
        queue.Enqueue(pos, dist)
        found.Add(pos, { FromPos = None; Distance = dist; IsExpanded = false })
    while not halt && queue.Count > 0 do
        let pos = queue.Dequeue()
        let node = found[pos]
        if not node.IsExpanded then
            found[pos] <- { node with IsExpanded = true }
            match expand pos node.Distance with
            | Neighbors ns -> 
                for npos, ndist in ns do
                    let enqueue = 
                        match found.TryGetValue(npos) with
                        | false, _ -> true
                        | true, otherNode -> ndist < otherNode.Distance
                    if enqueue then
                        queue.Enqueue(npos, ndist)
                        found.Add(npos, { FromPos = Some pos; Distance = ndist; IsExpanded = false})
            | StopSearch ->
                halt <- true
    found |> Seq.map (|KeyValue|) |> Map.ofSeq

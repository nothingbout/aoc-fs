module Utils.Search
open Utils
open Utils.Globals

module BFS = 
    type ExpandResult<'a> =
    | StopSearch
    | Neighbors of 'a seq

    type PathNode<'a> = { FromPos : 'a option; Distance : int }

    module PathNode = 
        let distance node = node.Distance

    let search (expand : 'a -> 'a ExpandResult) (positions : 'a seq) : Map<'a, 'a PathNode> =
        let mutable halt = false
        let queue = System.Collections.Generic.Queue<_>()
        let found = System.Collections.Generic.Dictionary<_, _>()
        for pos in positions do
            queue.Enqueue(pos)
            found.Add(pos, { FromPos = None; Distance = 0 })
        while not halt && queue.Count > 0 do
            let pos = queue.Dequeue()
            let dist = found[pos].Distance
            match expand pos with
            | Neighbors ns -> 
                for npos in ns do
                    if not (found.ContainsKey(npos)) then
                        queue.Enqueue(npos)
                        found.Add(npos, { FromPos = Some pos; Distance = dist + 1})
            | StopSearch ->
                halt <- true
        found |> Seq.map (|KeyValue|) |> Map.ofSeq

    let getPath toPos foundNodes = 
        Some toPos 
        |> Seq.unfold (fun posMaybe -> 
            match posMaybe with
            | Some pos -> Some (foundNodes |> Map.find pos |> fun node -> (pos, node.FromPos))
            | None -> None
        ) |> List.ofSeq |> List.rev

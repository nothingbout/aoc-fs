namespace Utils
open Globals

module GridMap = 
    let parseMap markers lines = 
        seq { 
            for y, line in lines |> List.indexed do
                for x, c in line |> Seq.indexed do
                    if Seq.contains c markers then
                        yield (c, Vec2.make x y)
        } |> Map.ofSeq

    let parseMapSet marker lines = 
        seq { 
            for y, line in lines |> List.indexed do
                for x, c in line |> Seq.indexed do
                    if c = marker then
                        yield (Vec2.make x y)
        } |> Set.ofSeq

    let parseMapToken token lines = 
        seq { 
            for y, line in lines |> List.indexed do
                for x, c in line |> Seq.indexed do
                    if c = token then
                        yield Vec2.make x y
        } |> Seq.head

    let printLines lines = 
        lines |> List.iter (printfn "%s")

    let toLinesWithFunc (func : Vec2<int> -> string) (bounds : Rect<int>) : string list =
        [for y in IntRange.toSeq bounds.YR ->
            [for x in IntRange.toSeq bounds.XR ->
                Vec2.make x y |> func
            ] |> String.concat ""]

    let toLines (bounds : Rect<int>) (emptySpace : string) (map : Map<Vec2<int>, string>) : string list =
        bounds |> toLinesWithFunc (fun pos -> 
            match map |> Map.tryFind pos with
            | None -> emptySpace
            | Some value -> value
        )

    let toLinesAutoBounds emptyChar map = 
        toLines (Map.keys map |> Rect.encapsulating) emptyChar map

type TraversalAxis = { PosArr : int array }

module TraversalAxis = 
    let private make posArr = { PosArr = posArr }
    let empty = make Array.empty
    let ofUniquePositions intSeq = let arr = intSeq |> Array.ofSeq in arr |> Array.sortInPlace; make arr
    let addPosition pos axis = axis.PosArr |> Array.insertSortedUnique pos |> make

    let tryFindBack fromPos axis = 
        let posArr = axis.PosArr
        match posArr |> Array.binarySearchLessOrEqual fromPos with
        | Some idx -> Some posArr[idx]
        | None -> None

    let tryFindForward fromPos axis = 
        let posArr = axis.PosArr
        match posArr |> Array.binarySearchLessOrEqual fromPos with
        | Some idx -> 
            if posArr[idx] = fromPos then Some fromPos
            else if idx < Array.length posArr - 1 then Some posArr[idx + 1]
            else None
        | None -> 
            if Array.length posArr > 0 then Some posArr[0]
            else None

type TraversalGrid = { Bounds : Rect<int>; XsByRow : TraversalAxis array; YsByCol : TraversalAxis array }

module TraveralGrid = 
    let make bounds positions = 
        let positions = Set.ofSeq positions
        let xsByRow = 
            IntRange.toSeq bounds.YR |> Seq.map (fun y ->
                IntRange.toSeq bounds.XR 
                |> Seq.filter (fun x -> Set.contains (Vec2.make x y) positions)
                |> TraversalAxis.ofUniquePositions
            ) |> Array.ofSeq
        let ysByCol = 
            IntRange.toSeq bounds.XR |> Seq.map (fun x ->
                IntRange.toSeq bounds.YR 
                |> Seq.filter (fun y -> Set.contains (Vec2.make x y) positions)
                |> TraversalAxis.ofUniquePositions
            ) |> Array.ofSeq
        { Bounds = bounds; XsByRow = xsByRow; YsByCol = ysByCol }
    
    let xsForY y grid = 
        let row = y - grid.Bounds.YR.Start
        if row < 0 || row >= Array.length grid.XsByRow then None
        else Some grid.XsByRow[row]

    let ysForX x grid = 
        let col = x - grid.Bounds.XR.Start
        if col < 0 || col >= Array.length grid.YsByCol then None
        else Some grid.YsByCol[col]

    let addPosition pos grid = 
        if not (Rect.contains pos grid.Bounds) then failwith $"{pos} not in bounds {grid.Bounds}"
        let { X = col; Y = row } = pos - Rect.start grid.Bounds
        let xsByRow = Array.copy grid.XsByRow
        xsByRow[row] <- grid.XsByRow[row] |> TraversalAxis.addPosition pos.X
        let ysByCol = Array.copy grid.YsByCol
        ysByCol[col] <- grid.YsByCol[col] |> TraversalAxis.addPosition pos.Y
        { Bounds = grid.Bounds; XsByRow = xsByRow; YsByCol = ysByCol }
 
    let tryFindInDirection fromPos dir grid = 
        if dir.X <> 0 && dir.Y <> 0 then failwith $"direction not along an axis {dir}"
        let { X = col; Y = row } = fromPos - Rect.start grid.Bounds
        if not (Rect.contains fromPos grid.Bounds) then None
        else if dir.X < 0 then
            xsForY fromPos.Y grid >>= TraversalAxis.tryFindBack fromPos.X |?> fun x -> Vec2.make x fromPos.Y
        else if dir.X > 0 then
            xsForY fromPos.Y grid >>= TraversalAxis.tryFindForward fromPos.X |?> fun x -> Vec2.make x fromPos.Y
        else if dir.Y < 0 then
            ysForX fromPos.X grid >>= TraversalAxis.tryFindBack fromPos.Y |?> fun y -> Vec2.make fromPos.X y
        else if dir.Y > 0 then
            ysForX fromPos.X grid >>= TraversalAxis.tryFindForward fromPos.Y |?> fun y -> Vec2.make fromPos.X y
        else
            failwith $"invalid direction {dir}"

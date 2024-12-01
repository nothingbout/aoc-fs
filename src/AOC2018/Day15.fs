module AOC2018.Day15
open Utils
open Utils.Globals

[<Struct>]
type Unit = { Type : char; HP : int; Power : int }

module Unit =
    let canAttack a b = a.Type <> b.Type

let parseMap lines = 
    let map = Array2.fromStringLines lines
    let mutable units = Map.empty
    map |> Array2.iteri (fun pos c ->
        match c with
        | 'E' | 'G' -> 
            map[pos] <- '.'
            units <- units |> Map.add pos { Type = c; HP = 200; Power = 3 }
        | _ -> ()
    )
    (map, units)

let printMap (func : Vec2<int> -> string option) map = 
    IntRect.withSize (Vec2.makeZero ()) (Array2.dims map) 
    |> GridMap.toLinesWithFunc (fun pos ->
        match func pos with
        | Some str -> str
        | None -> map[pos] |> toString
    ) 
    |> GridMap.printLines

let sortPositions positions = 
    positions |> List.ofSeq |> List.sortWith Vec2.readingOrder

let printMapWithUnits map units = 
    map |> printMap (fun pos ->
        match units |> Map.tryFind pos with
        | Some unit -> Some (unit.Type |> toString)
        | None -> None
    )

let printUnitHPs units = 
    units |> Map.keys |> sortPositions |> Seq.iter (fun pos -> 
        let unit = units |> Map.find pos
        printfn $"{pos}: {unit.Type} {unit.HP}"
    )

let printSearchResult map foundNodes = 
    map |> printMap (fun pos ->
        match foundNodes |> Map.tryFind pos with
        | Some node -> Some (Search.BFS.PathNode.distance node % 10 |> toString)
        | None -> None
    )

let adjacentPositions (pos : Vec2<int>) = 
    // Ordered in reading order (important for path priorization)
    seq { pos + Vec2.make 0 -1; pos + Vec2.make -1 0; pos + Vec2.make 1 0; pos + Vec2.make 0 1 }

let canMoveTo map units toPos =
    map |> Array2.get toPos '#' = '.' && not (units |> Map.containsKey toPos)

let tryFindBestPath map units targetPositions fromPos =
    let foundNodes = [fromPos] |> Search.BFS.search (fun pos ->
        if targetPositions |> Set.contains pos then 
            Search.BFS.StopSearch
        else
            Search.BFS.Neighbors (pos |> adjacentPositions |> Seq.filter (fun npos -> canMoveTo map units npos))
    )
    let reachable = targetPositions |> Seq.filter (fun pos -> foundNodes |> Map.containsKey pos)
    if Seq.isEmpty reachable then None
    else
    reachable 
    |> Seq.minBy (fun pos -> ((foundNodes |> Map.find pos).Distance, pos.Y, pos.X))
    |> fun pos -> foundNodes |> Search.BFS.getPath pos |> Some

let getUnitTargetPositions unit units = 
    units |> Seq.collect (fun (KeyValue (otherPos, otherUnit)) ->
        if not (Unit.canAttack unit otherUnit) then Seq.empty
        else otherPos |> adjacentPositions
    ) |> Set.ofSeq

let shouldCombatEnd units = 
    units |> Map.values |> Seq.map (fun unit -> unit.Type) |> Seq.distinct |> Seq.length = 1

let moveUnit map targetPositions unit unitPos units =
    if targetPositions |> Set.contains unitPos then unitPos, units
    else
    match tryFindBestPath map units targetPositions unitPos with
    | Some (_ :: newPos :: _path) ->
        // printfn $"Moving to {List.last path} via {npos}"
        newPos, units |> Map.remove unitPos |> Map.add newPos unit
    | _ -> unitPos, units

let tryFindAdjacentUnitToAttack unit unitPos units =
    unitPos |> adjacentPositions |> Seq.choose (fun otherPos ->
        match units |> Map.tryFind otherPos with 
        | Some otherUnit -> if Unit.canAttack unit otherUnit then Some (otherPos, otherUnit) else None
        | None -> None
    ) |> Seq.sortBy (fun (otherPos, otherUnit) -> (otherUnit.HP, otherPos.Y, otherPos.X)) |> Seq.tryHead

let attackWithUnit unit unitPos units = 
    match units |> tryFindAdjacentUnitToAttack unit unitPos with
    | Some (otherPos, otherUnit) -> 
        let newHP = otherUnit.HP - unit.Power
        // printfn $"{unitPos} {unit.Type} attacks {otherPos} {otherUnit.Type} and puts its HP at {newHP}"
        if newHP <= 0 then 
            units |> Map.remove otherPos
        else 
            units |> Map.add otherPos { otherUnit with HP = newHP }
    | None -> units

let updateUnit map unit unitPos units = 
    let targetPositions = units |> getUnitTargetPositions unit
    if Set.isEmpty targetPositions then true, units
    else
    let unitPos, units = (unitPos, units) ||> moveUnit map targetPositions unit
    let units = units |> attackWithUnit unit unitPos
    false, units

let rec playRound map order units = 
    match order with
    | [] -> false, units
    | unitPos :: order ->
        match units |> Map.tryFind unitPos with
        | None -> units |> playRound map order
        | Some unit ->
            let endCombat, units = units |> updateUnit map unit unitPos
            if endCombat then 
                true, units
            else 
                units |> playRound map order

let rec playRoundsUntilEnd additionalEndCond map completedRounds units =
    // printfn $"After {completedRounds} rounds:"
    // printMapWithUnits map units
    // printUnitHPs units
    // printfn ""
    let order = Map.keys units |> sortPositions
    let endCombat, units = units |> playRound map order
    if endCombat || additionalEndCond units then 
        (completedRounds, units)
    else 
        (completedRounds + 1, units) ||> playRoundsUntilEnd additionalEndCond map

let totalHP units = units |> Map.values |> Seq.sumBy (fun unit -> unit.HP)
let countElves units = units |> Map.filter (fun _ unit -> unit.Type = 'E') |> Map.count
let setElvesPower power units = 
    units |> Map.map (fun _ unit -> if unit.Type = 'E' then { unit with Power = power } else unit)

let solveP1 (inputLines: string list) = 
    let map, units = inputLines |> parseMap
    let rounds, units = (0, units) ||> playRoundsUntilEnd (fun _ -> false) map
    rounds * totalHP units |> Answer.int
    
let solveP2 (inputLines: string list) = 
    let map, units = inputLines |> parseMap
    let initialElfCount = countElves units
    Seq.initInfinite (fun i -> 4 + i) |> Seq.pick (fun power -> 
        let rounds, units = 
            (map, 0, units |> setElvesPower power) 
            |||> playRoundsUntilEnd (fun units -> 
                if countElves units < initialElfCount then true else false)
        if countElves units = initialElfCount 
            then Some (rounds * totalHP units)
        else 
            None
    ) |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example1.txt" (Answer.int 27730)
        Puzzle.create solveP1 "Part 1" "example2.txt" (Answer.int 36334)
        Puzzle.create solveP1 "Part 1" "example3.txt" (Answer.int 39514)
        Puzzle.create solveP1 "Part 1" "example4.txt" (Answer.int 27755)
        Puzzle.create solveP1 "Part 1" "example5.txt" (Answer.int 28944)
        Puzzle.create solveP1 "Part 1" "example6.txt" (Answer.int 18740)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 248235)
        Puzzle.create solveP2 "Part 2" "example1.txt" (Answer.int 4988)
        Puzzle.create solveP2 "Part 2" "example3.txt" (Answer.int 31284)
        Puzzle.create solveP2 "Part 2" "example4.txt" (Answer.int 3478)
        Puzzle.create solveP2 "Part 2" "example5.txt" (Answer.int 6474)
        Puzzle.create solveP2 "Part 2" "example6.txt" (Answer.int 1140)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 0)
    ]

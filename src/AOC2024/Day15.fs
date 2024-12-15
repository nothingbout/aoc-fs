module AOC2024.Day15
open Utils
open Utils.Globals

let parseMap w lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                if c = '#' then
                    for i = 0 to w - 1 do
                        yield (Vec2.make (w * x + i) y, c)
                else if c = 'O' then
                    yield (Vec2.make (w * x) y, c)
    } |> Map.ofSeq

let parseRobot w lines = 
    seq { 
        for y, line in lines |> List.indexed do
            for x, c in line |> Seq.indexed do
                if c = '@' then
                    yield (Vec2.make (w * x) y)
    } |> Seq.head

let parseMoves lines = 
    lines |> String.concat "" |> List.ofSeq

let moveToVec move = 
    match move with 
    | '>' -> Vec2.make 1 0
    | '^' -> Vec2.make 0 -1
    | '<' -> Vec2.make -1 0
    | 'v' -> Vec2.make 0 1
    | _ -> failwith $"unexpected move {move}"

let isWall pos map = map |> Map.get pos '.' = '#'

let tryGetBox map w pos = 
    seq { 0..w - 1 } |> Seq.tryPick (fun i ->
        let boxPos = pos + Vec2.make -i 0
        if map |> Map.get boxPos '.' = 'O' then Some boxPos
        else None
    )

let moveBox map pos move =
    map |> Map.remove pos |> Map.add (pos + moveToVec move) 'O'

let rec tryPush map w pos move = 
    if isWall pos map then None else 
    match tryGetBox map w pos with
    | None -> Some map
    | Some boxPos -> 
        let nextPositions = 
            match move with
            | '>' -> [boxPos + Vec2.make w 0] 
            | '<' -> [boxPos + Vec2.make -1 0]
            | '^' -> [0..w - 1] |> List.map (fun i -> boxPos + Vec2.make i -1) 
            | 'v' -> [0..w - 1] |> List.map (fun i -> boxPos + Vec2.make i 1)
            | _ -> failwith $"unexpected move {move}"

        (Some map, nextPositions) ||> Seq.fold (fun maybeMap nextPos ->
            maybeMap |> Option.bind (fun map -> tryPush map w nextPos move)
        ) |> Option.map (fun map -> moveBox map boxPos move) 

let moveRobot map w pos move = 
    let nextPos = pos + moveToVec move
    match tryPush map w nextPos move with
    | Some newMap -> (newMap, nextPos)
    | None -> (map, pos)

let printMap map w robotPos move moves = 
    let lines = 
        Rect.encapsulating (Map.keys map)
        |> GridMap.toLinesWithFunc (fun pos -> 
            if pos = robotPos then "@" else
            if isWall pos map then "#" else
            match tryGetBox map w pos with
            | Some boxPos -> 
                if w = 1 then "O" else
                if boxPos = pos then "[" else "]"
            | None -> "."
        )
    System.Console.Clear();
    printfn $"Move {move}/{moves}: {move}"
    lines |> GridMap.printLines
    System.Threading.Thread.Sleep(20)

let solve w inputLines = 
    let mapLines, moveLines = inputLines |> List.splitAtFirst (fun line -> line = "")
    let map = mapLines |> parseMap w
    let robotPos = mapLines |> parseRobot w
    let moves = moveLines |> parseMoves

    ((map, robotPos), moves |> List.indexed)
    ||> List.fold (fun (map, robotPos) (i, move) ->
        // printMap map w robotPos i (List.length moves)
        moveRobot map w robotPos move
    )
    |> fst |> Map.filter (fun _ v -> v = 'O') |> Map.keys 
    |> Seq.sumBy (fun p -> p.Y * 100 + p.X) |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create (solve 1) "Part 1" "example.txt" (Answer.int 10092)
        Puzzle.create (solve 1) "Part 1" "example2.txt" (Answer.int 2028)
        Puzzle.create (solve 1) "Part 1" "input.txt" (Answer.int 1398947)
        Puzzle.create (solve 2) "Part 2" "example.txt" (Answer.int 9021)
        Puzzle.create (solve 2) "Part 2" "input.txt" (Answer.int 1397393)
    ]

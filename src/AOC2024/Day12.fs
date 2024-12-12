module AOC2024.Day12
open Utils
open Utils.Globals

let parseMap lines = 
    Array2.ofStringLines lines
    
let isSamePlant map pos npos = 
    if Array2.contains pos map && Array2.contains npos map then map[pos] = map[npos]
    else false

let rec floodfill (expand : Vec2<int> -> Vec2<int> seq) visited pos = 
    Dict.add visited pos true
    for npos in expand pos do
        if not (Dict.containsKey visited npos) then 
            (visited, npos) ||> floodfill expand
        else ()

let findRegions map = 
    let visitedPlots = Dict<_, _>()
    let mutable regions = []
    for startPos in Array2.indices map do
        if not (Dict.containsKey visitedPlots startPos) then
            let mutable plots = []
            let mutable fences = []
            (visitedPlots, startPos) ||> floodfill (fun pos -> 
                plots <- pos :: plots

                Vec2.dir4 |> Seq.map (fun dir -> pos + dir) 
                |> Seq.filter (fun npos -> 
                    if isSamePlant map pos npos then
                        true
                    else
                        fences <- (pos, npos - pos) :: fences
                        false) 
            )
            regions <- (plots, fences) :: regions
    regions

let rec countSides fences pos dir nsides visited = 
    if visited |> Set.contains (pos, dir) then nsides, visited
    else
    let nextPos, nextDir = 
        seq {
            yield pos, Vec2.turnCW dir
            yield pos + Vec2.turnCW dir, dir
            yield pos + Vec2.turnCW dir + dir, Vec2.turnCCW dir
        } |> Seq.find (fun f -> Set.contains f fences)

    if nsides = 0 && dir = nextDir then
        // skip until first turn in order to not split the starting side in two parts
        countSides fences nextPos nextDir nsides visited
    else
        countSides fences nextPos nextDir 
            (if nextDir <> dir then nsides + 1 else nsides) 
            (visited |> Set.add (pos, dir))

let solveP1 (inputLines: string list) = 
    let map = inputLines |> parseMap
    findRegions map |> Seq.sumBy (fun (plots, fences) ->
        List.length plots * List.length fences
    ) |> Answer.int

let solveP2 (inputLines: string list) = 
    let map = inputLines |> parseMap
    findRegions map |> Seq.sumBy (fun (plots, fences) ->
        let mutable fences = Set.ofList fences
        let mutable totalSides = 0
        while not (Set.isEmpty fences) do
            let pos, dir = fences |> Set.toList |> List.head
            let sides, visitedFences = countSides fences pos dir 0 Set.empty
            fences <- Set.difference fences visitedFences
            totalSides <- totalSides + sides
        List.length plots * totalSides
    ) |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int 140)
        Puzzle.create solveP1 "Part 1" "example1_2.txt" (Answer.int 772)
        Puzzle.create solveP1 "Part 1" "example1_3.txt" (Answer.int 1930)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int 1573474)
        Puzzle.create solveP2 "Part 2" "example.txt" (Answer.int 80)
        Puzzle.create solveP2 "Part 2" "example2_2.txt" (Answer.int 236)
        Puzzle.create solveP2 "Part 2" "example2_3.txt" (Answer.int 368)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 966476)
    ]

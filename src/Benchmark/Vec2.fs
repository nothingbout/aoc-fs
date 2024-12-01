namespace Benchmark
open Utils

module Vec2AsTuple = 
    type Vec2 = (int * int)

    module Vec2 = 
        let inline make x y : Vec2 = (x, y)
        let add ((ax, ay) : Vec2) ((bx, by) : Vec2) = make (ax + bx) (ay + by)

    let benchmark inputLines = 
        let arrLen = inputLines |> List.head |> int
        let arr = Array.init arrLen (fun i -> Vec2.make (i + 0) (i + 1))
        arr |> Array.fold (fun acc v -> Vec2.add acc v |> Vec2.add v |> Vec2.add v |> Vec2.add v) (Vec2.make 0 0)
        |> fun (vx, vy) -> vx + vy |> Answer.int

module Vec2AsRecord = 
    type Vec2 = {X : int; Y : int}

    module Vec2 = 
        let inline make x y : Vec2 = {X = x; Y = y}
        let add (a : Vec2) (b : Vec2) = make (a.X + b.X) (a.Y + b.Y)

    let benchmark inputLines = 
        let arrLen = inputLines |> List.head |> int
        let arr = Array.init arrLen (fun i -> Vec2.make (i + 0) (i + 1))
        arr |> Array.fold (fun acc v -> Vec2.add acc v |> Vec2.add v |> Vec2.add v |> Vec2.add v) (Vec2.make 0 0)
        |> fun v -> v.X + v.Y |> Answer.int

module Vec2AsStructRecord = 
    [<Struct>]
    type Vec2 = {X : int; Y : int}

    module Vec2 = 
        let inline make x y : Vec2 = {X = x; Y = y}
        let add (a : Vec2) (b : Vec2) = make (a.X + b.X) (a.Y + b.Y)

    let benchmark inputLines = 
        let arrLen = inputLines |> List.head |> int
        let arr = Array.init arrLen (fun i -> Vec2.make (i + 0) (i + 1))
        arr |> Array.fold (fun acc v -> Vec2.add acc v |> Vec2.add v |> Vec2.add v |> Vec2.add v) (Vec2.make 0 0)
        |> fun v -> v.X + v.Y |> Answer.int

module Vec2AsStruct = 
    [<Struct>]
    type Vec2 = 
        val X : int
        val Y : int
        new (x, y) = { X = x; Y = y }

    module Vec2 = 
        let inline make x y = Vec2(x, y)
        let add (a : Vec2) (b : Vec2) = Vec2(a.X + b.X, a.Y + b.Y)

    let benchmark inputLines = 
        let arrLen = inputLines |> List.head |> int
        let arr = Array.init arrLen (fun i -> Vec2.make (i + 0) (i + 1))
        arr |> Array.fold (fun acc v -> Vec2.add acc v |> Vec2.add v |> Vec2.add v |> Vec2.add v) (Vec2.make 0 0)
        |> fun v -> v.X + v.Y |> Answer.int

module Vec2Benchmark = 
    let measure fn = 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let answer = fn()
        sw.Stop()
        printfn $"Answer: {answer}, Elapsed: %.2f{sw.Elapsed.TotalMilliseconds} ms"

    let getPuzzles() = 
        [
            let answer = Answer.int 1385447424
            Puzzle.create Vec2AsTuple.benchmark "Tuple" "input.txt" answer
            Puzzle.create Vec2AsRecord.benchmark "Record" "input.txt" answer
            Puzzle.create Vec2AsStructRecord.benchmark "StructRecord" "input.txt" answer
            Puzzle.create Vec2AsStruct.benchmark "Struct" "input.txt" answer
        ]

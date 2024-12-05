namespace Utils
open Globals

[<Struct>]
type Vec2<'T> = {X : 'T; Y : 'T}
    with 
        override this.ToString (): string = $"({this.X}, {this.Y})"
        static member inline get_Zero () = { X = makeZero (); Y = makeZero () }
        static member inline ( ~- ) (a) = {X = -a.X; Y = -a.Y}
        static member inline ( + ) (a, b) = {X = a.X + b.X; Y = a.Y + b.Y}
        static member inline ( - ) (a, b) = {X = a.X - b.X; Y = a.Y - b.Y}
        static member inline ( * ) (a : Vec2<'a>, s : 'a) = {X = a.X * s; Y = a.Y * s}
        static member inline ( * ) (a, b : Vec2<_>) = {X = a.X * b.X; Y = a.Y * b.Y}
        static member inline ( / ) (a : Vec2<'a>, s : 'a) = {X = a.X / s; Y = a.Y / s}
        static member inline ( / ) (a, b : Vec2<_>) = {X = a.X / b.X; Y = a.Y / b.Y}

module Vec2 = 
    let inline make x y = {X = x; Y = y}
    let inline makeZero () = make (makeZero ()) (makeZero ())
    let inline ofTuple (x, y) = make x y
    let inline map f a = make (f a.X) (f a.Y)
    let inline min a b = make (min a.X b.X) (min a.Y b.Y)
    let inline max a b = make (max a.X b.X) (max a.Y b.Y)
    let inline minElem a = min a.X a.Y
    let inline maxElem a = max a.X a.Y
    let inline sign a = make (sign a.X) (sign a.Y)
    let inline abs a = make (abs a.X) (abs a.Y)
    let inline sum a = a.X + a.Y
    let inline product a = a.X * a.Y
    let inline dot a b = a.X * b.X + a.Y * b.Y

    let readingOrder a b =
        if a.Y < b.Y then -1
        else if a.Y > b.Y then 1
        else if a.X < b.X then -1
        else if a.X > b.X then 1
        else 0

    let inline turnCCW a = make -a.Y a.X
    let inline turnCW a = make a.Y -a.X
    let inline fromDegrees d = make (System.Math.Cos(d), System.Math.Sin(d))
    let inline degrees a = System.Math.Atan2(a.Y, a.X)
    let inline degreesDeltaPositive src dst = (dst - src) %+ 360.0
    let inline degreesDeltaNegative src dst = degreesDeltaPositive src dst - 360.0
    let inline degreesDelta src dst = degreesDeltaPositive src dst |> fun d -> if d <= 180.0 then d else d - 360.0

    let dir4 = [ make 1 0; make 0 -1; make -1 0; make 0 1 ]
    let diag4 = [ make 1 -1; make -1 -1; make -1 1; make 1 1 ]
    let dir8 = List.zip dir4 diag4 |> List.map (fun (a, b) -> [a; b]) |> List.concat

[<Struct>]
type Rect<'T> = { XR : Range<'T>; YR : Range<'T> }

module Rect = 
    let inline make start finish = { XR = Range.make start.X finish.X; YR = Range.make start.Y finish.Y }
    let inline start r = Vec2.make (Range.start r.XR) (Range.start r.YR)
    let inline finish r = Vec2.make (Range.finish r.XR) (Range.finish r.YR)

    let inline contains p r = Range.contains r.XR p.X && Range.contains r.YR p.Y

    let inline intersect a b = 
        maybe {
            let! xr = Range.intersect a.XR b.XR
            let! yr = Range.intersect a.YR b.YR
            return { XR = xr; YR = yr }
        }

    let encapsulating points = make (points |> Seq.reduce Vec2.min) (points |> Seq.reduce Vec2.max)

    let inline expand amount r = make (start r - amount) (finish r + amount)

module IntRect =
    let inline withSize start size = { XR = IntRange.withLength start.X size.X; YR = IntRange.withLength start.Y size.Y }
    let inline width r = IntRange.length r.XR
    let inline height r = IntRange.length r.YR
    let inline size r = Vec2.make (width r) (height r)

    let pointsByRow r = 
        seq {
            for y = r.YR.Start to r.YR.Finish do
                for x = r.XR.Start to r.XR.Finish do
                    yield Vec2.make x y
        } 

// module TestVec2Range =
//     let testInt = 
//         let range = Range.make (Vec2.make 0 0) (Vec2.make 10 10)
//         let range2 = Range.make (Vec2.make 5 5) (Vec2.make 15 15)
//         let range3 = Range.intersect range range2
//         ()

module GridMap = 
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

type Array2<'a>(dims : Vec2<int>, buffer : 'a array) = 
    member _.Dims = dims
    member _.Buffer = buffer
    member this.Item
        with inline get (pos) = this.Buffer[pos.Y * this.Dims.X + pos.X]
        and inline set pos value = this.Buffer[pos.Y * this.Dims.X + pos.X] <- value

module Array2 = 
    let inline dims (source : Array2<'a>) = source.Dims
    let inline buffer (source : Array2<'a>) = source.Buffer
    let inline width source = (dims source).X
    let inline height source = (dims source).Y
    let inline bufferIdx pos source = pos.Y * (width source) + pos.X

    let make dims buffer = Array2(dims, buffer)
    let zeroCreate dims = make dims (dims.X * dims.Y |> Array.zeroCreate)
    let copy source = make (dims source) (buffer source |> Array.copy)

    let contains pos source = pos.X >= 0 && pos.Y >= 0 && pos.X < width source && pos.Y < height source
    let get pos orDefault source = if source |> contains pos then source[pos] else orDefault
    let map mapping source = make (dims source) (buffer source |> Array.map mapping)
    let fold folder state source = buffer source |> Array.fold folder state
    let sum source = source |> fold (+) 0

    let mapi mapping source = 
        seq {
            for y = 0 to height source - 1 do
                for x = 0 to width source - 1 do
                    let pos = Vec2.make x y
                    yield mapping pos source[pos]
        } |> Array.ofSeq |> make (dims source)

    let foldi folder state source = 
        let mutable state = state
        for y = 0 to height source - 1 do
            for x = 0 to width source - 1 do
                let pos = Vec2.make x y
                state <- folder state pos source[pos]
        state

    let iteri action source = 
        for y = 0 to height source - 1 do
            for x = 0 to width source - 1 do
                let pos = Vec2.make x y
                action pos source[pos]

    let row y source = 
        let start = Vec2.make 0 y
        let finish = Vec2.make (width source - 1) y
        (buffer source)[bufferIdx start source..bufferIdx finish source]

    let rows source = 
        let height = height source
        let rows = Array.zeroCreate height
        for y = 0 to height - 1 do rows[y] <- source |> row y
        rows

    let fromRows rows = 
        if Array.isEmpty rows then make (Vec2.makeZero ()) Array.empty
        else
        let width = Array.length rows[0]
        let height = Array.length rows
        if not (Array.forall (fun row -> Array.length row = width) rows) then failwith $"all rows should have the same length"
        make (Vec2.make width height) (Array.concat rows)

    let fromStringLines lines = 
        lines |> Seq.map String.toArray |> Array.ofSeq |> fromRows

    let toStringLines source = 
        source |> rows |> Array.map String.ofArray


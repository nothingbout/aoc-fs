namespace Utils
open Globals

[<Struct>]
type Vec<'T> = { Arr : 'T array }
    with 
        member this.Item
            with inline get (idx) = this.Arr[idx]
            and inline set idx value = this.Arr[idx] <- value
        override this.ToString (): string = sprintf "(%s)" (this.Arr |> Seq.map (fun x -> x.ToString()) |> String.concat ", ")
        // static member inline get_Zero () = { X = makeZero (); Y = makeZero () }
        static member inline ( ~- ) (a) = {Arr = Array.map ( ~- ) a.Arr}
        static member inline ( + ) (a : Vec<_>, b : Vec<_>) : Vec<_> = {Arr = Array.map2 ( + ) a.Arr b.Arr}
        static member inline ( - ) (a : Vec<_>, b : Vec<_>) : Vec<_> = {Arr = Array.map2 ( - ) a.Arr b.Arr}
        static member inline ( * ) (a : Vec<'a>, s : 'a) : Vec<_> = {Arr = Array.map (fun x -> x * s) a.Arr}
        static member inline ( * ) (a : Vec<_>, b : Vec<_>) : Vec<_> = {Arr = Array.map2 ( * ) a.Arr b.Arr}
        static member inline ( / ) (a : Vec<'a>, s : 'a) : Vec<_> = {Arr = Array.map (fun x -> x / s) a.Arr}
        static member inline ( / ) (a : Vec<_>, b : Vec<_>) : Vec<_> = {Arr = Array.map2 ( / ) a.Arr b.Arr}

module Vec = 
    let inline ofArray arr = {Arr = arr}
    let inline toArray v = v.Arr
    let inline item (v : Vec<_>) i = v[i]
    let inline make2 x y = ofArray [|x; y|]
    let inline make3 x y z = ofArray [|x; y; z|]
    let inline make4 x y z w = ofArray [|x; y; z; w|]
    let inline makeZero n = Array.zeroCreate n |> ofArray
    let inline map f a = Array.map f (toArray a) |> ofArray
    let inline map2 f a b = Array.map2 f (toArray a) (toArray b) |> ofArray
    let inline reduce f a = toArray a |> Array.reduce f
    let inline min a b = map2 min a b
    let inline max a b = map2 max a b
    let inline minElem a = reduce min a
    let inline maxElem a = reduce max a
    let inline sign a = map sign a
    let inline abs a = map abs a
    let inline sum a = reduce (+) a
    let inline product a = reduce ( * ) a
    let inline dot a b = a * b |> sum

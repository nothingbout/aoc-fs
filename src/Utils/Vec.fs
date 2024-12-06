namespace Utils
open Globals

[<Struct>]
type Vec<'T> = { Arr : 'T array }
    with 
        member this.Item
            with inline get (idx) = this.Arr[idx]
            // and inline set idx value = this.Arr[idx] <- value
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
    let inline wrapArray arr = {Arr = arr}
    let inline unwrapArray v = v.Arr

    let inline ofArray arr = {Arr = Array.copy arr}
    let inline toArray v = Array.copy v.Arr
    let inline ofList list = Array.ofList list |> wrapArray
    let inline toList v = unwrapArray v |> List.ofArray 
    let inline ofSeq seq = Array.ofSeq seq |> wrapArray
    let inline item (v : Vec<_>) idx = v[idx]
    let inline withItem idx value v = let arr = toArray v in Array.set arr idx value; arr
    let inline make2 x y = wrapArray [|x; y|]
    let inline make3 x y z = wrapArray [|x; y; z|]
    let inline make4 x y z w = wrapArray [|x; y; z; w|]
    let inline makeZero n = Array.zeroCreate n |> wrapArray
    let inline init n initializer = Array.init n initializer |> wrapArray
    let inline map f a = Array.map f (unwrapArray a) |> wrapArray
    let inline map2 f a b = Array.map2 f (unwrapArray a) (unwrapArray b) |> wrapArray
    let inline mapi f a = Array.mapi f (unwrapArray a) |> wrapArray
    let inline mapi2 f a b = Array.mapi2 f (unwrapArray a) (unwrapArray b) |> wrapArray
    let inline reduce f a = unwrapArray a |> Array.reduce f
    let inline min a b = map2 min a b
    let inline max a b = map2 max a b
    let inline minElem a = reduce min a
    let inline maxElem a = reduce max a
    let inline sign a = map sign a
    let inline abs a = map abs a
    let inline sum a = reduce (+) a
    let inline product a = reduce ( * ) a
    let inline dot a b = a * b |> sum

    let axisDirs n = 
        seq { for i = 0 to n - 1 do yield Array.init n (fun j -> if j = i then 1 else 0)
              for i = 0 to n - 1 do yield Array.init n (fun j -> if j = i then -1 else 0) }

    let rec private diagonalDirLists n = 
        if n = 0 then Seq.singleton []
        else seq {  for tail in diagonalDirLists (n - 1) do yield 1 :: tail
                    for tail in diagonalDirLists (n - 1) do yield -1 :: tail }
    let diagonalDirs n = diagonalDirLists n |> Seq.map ofList


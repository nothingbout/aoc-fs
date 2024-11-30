namespace Utils

module Globals =
    let inline makeZero () = LanguagePrimitives.GenericZero
    let inline makeOne () = LanguagePrimitives.GenericOne

    ///Calculates remainder that is always positive.
    let inline (%+) dividend divisor = 
        let r = dividend % divisor
        if r >= makeZero () then r else r + divisor

    let (>>=) ma f = Option.bind f ma
    
    type MaybeBuilder() =
        member _.Bind(m, f) = 
            Option.bind f m
        member _.Return(x) = 
            Some x
        member _.ReturnFrom(x) = 
            x

    let maybe = MaybeBuilder()

    let inline inspect x = printfn $"{x}"; x
    let inline inspectSeq seq = printfn $"Seq {List.ofSeq seq}"; seq

    let inline printn x = printfn $"{x}"

open Globals

[<Struct>]
type Range<'T> = {Start : 'T; Finish : 'T}

module Range = 
    let make start finish = {Start = start; Finish = finish}

    let start r = r.Start
    let finish r = r.Finish

    let inline contains r x = r.Start <= x && x <= r.Finish

    let inline union a b = 
        make (min a.Start b.Start) (max a.Finish b.Finish)
        
    let inline intersect a b = 
        let start = max a.Start b.Start
        let finish = min a.Finish b.Finish
        if start <= finish then Some (make start finish)
        else None

module IntRange = 
    let inline withLength start length = {Start = start; Finish = start + length - makeOne ()}
    let inline length r = r.Finish - r.Start + 1
    let inline toSeq r = seq { r.Start .. 1 .. r.Finish }

// module TestRange =
//     let testInt = 
//         let range = Range.make 0 10
//         let range2 = Range.make 5 15
//         let range3 = Range.intersect range range2
//         ()

//     let testBigint = 
//         let range = Range.make 0I 10I
//         let range2 = Range.make 5I 15I
//         let range3 = Range.intersect range range2
//         ()

module List = 
    let foldHead folder source = 
        List.fold folder (List.head source) (List.tail source)

    let rec permutationsSeq source = 
        let n = List.length source
        if n < 2 then Seq.singleton source
        else
        seq {
            for i = 0 to n - 1 do
                match List.splitAt i source with
                | xs, y :: ys -> 
                    for p in permutationsSeq (xs @ ys) do
                        yield y :: p
                | _ -> failwith "this never happens"
        }

    let permutations source = source |> permutationsSeq |> List.ofSeq

module Seq = 
    let foldHead folder source = 
        Seq.fold folder (Seq.head source) (Seq.tail source)

module Array = 
    let inline swap i j (source : 'a array) = 
        let tmp = source[i]
        source[i] <- source[j]
        source[j] <- tmp

    let iterPermutations action source = 
        let rec _iter action arr i = 
            if i = Array.length arr - 1 then action (Array.copy arr)
            for j = i to Array.length arr - 1 do
                swap i j arr
                _iter action arr (i + 1)
                swap i j arr
        _iter action (Array.copy source) 0

module Map =
    let inline get key defaultValue = 
        Map.tryFind key >> Option.defaultValue defaultValue

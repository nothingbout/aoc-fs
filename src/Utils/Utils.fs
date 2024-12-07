namespace Utils

module Globals =
    let inline makeZero () = LanguagePrimitives.GenericZero
    let inline makeOne () = LanguagePrimitives.GenericOne

    ///Calculates remainder that is always positive.
    let inline (%+) dividend divisor = 
        let r = dividend % divisor
        if r >= makeZero () then r else r + divisor

    let (>>=) ma f = Option.bind f ma
    let (|?>) ma f = Option.map f ma
    
    type MaybeBuilder() =
        member _.Bind(m, f) = 
            Option.bind f m
        member _.Return(x) = 
            Some x
        member _.ReturnFrom(x) = 
            x

    let maybe = MaybeBuilder()

    let inline fstv (struct (a, _)) = a
    let inline sndv (struct (_, b)) = b

    let inline toString x = x.ToString()
    let seqToString x = x |> Seq.map toString |> String.concat "; "
    let bigintOfString str = System.Numerics.BigInteger.Parse(str)

    let inline countIntegerDigits base' a = 
        let mutable digits = 1
        let mutable rem = a
        while rem >= base' do
            digits <- digits + 1
            rem <- rem / base'
        digits

    let inline concatIntegers base' a b = 
        // if a < makeZero () || b < makeZero () then failwith "expected positive integers but got {a} and {b}"
        let result = a * pown base' (countIntegerDigits base' b) + b
        // if result < makeZero() then failwith "expected a positive result but got {result}... overflow?"
        result

    let countDigits = countIntegerDigits 10
    let countDigitsInt64 = countIntegerDigits 10L
    let countDigitsBigint = countIntegerDigits 10I
    let concatInts = concatIntegers 10
    let concatInt64s = concatIntegers 10L
    let concatBigints = concatIntegers 10I

    let inspect x = printfn $"{x}"; x
    let inspectSeq seq = 
        printfn "seq {"
        seq |> Seq.iter (fun x -> printfn $"    {x}")
        printfn "}"
        seq

    let printn x = printfn $"{x}"
    let printSeq seq = inspectSeq seq |> ignore

open Globals

[<Struct>]
type Range<'T> = {Start : 'T; Finish : 'T}

module Range = 
    let inline make start finish = {Start = start; Finish = finish}

    let inline start r = r.Start
    let inline finish r = r.Finish
    let inline subFrom from r = 
        assert (from >= 0)
        make (r.Start + from) r.Finish
    let inline subTo to' r = 
        assert (r.Start + to' <= r.Finish)
        make r.Start (r.Start + to')
    let inline sub from to' r = 
        r |> subTo to' |> subFrom from

    let inline contains r x = r.Start <= x && x <= r.Finish

    let union a b = 
        make (min a.Start b.Start) (max a.Finish b.Finish)
        
    let intersect a b = 
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

module BinarySearch = 
    ///The collection must be sorted in ascending order as per the provided comparer, i.e.
    ///the comparer must return true for the sought for index and all indices lower than it.
    ///Returns the highest index for which the comparer returns true.
    ///Returns nil if the comparer returns false for the min index.
    let inline lastLessOrEqual isLEQ start finish =
        let mutable start = start
        let mutable finish = finish
        while start <> finish && start + makeOne () <> finish do
            let middle = (start + finish) / (makeOne () + makeOne ())
            if isLEQ middle then
                // mid may be the sought for index, since comparer returned true, so include it in search
                start <- middle
            else
                // mid can't be the sought for index, since comparer returned false, so exclude it from the search
                finish <- middle - makeOne ()                
        if isLEQ finish then Some finish
        else if start <> finish && isLEQ start then Some start
        else None

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

    let groupByAndMap keyProjection valueProjection source =
        source |> List.groupBy keyProjection |> List.map (fun (key, values) -> key, values |> List.map valueProjection)

    let groupByAndMapIntoSet keyProjection valueProjection source =
        source |> List.groupBy keyProjection |> List.map (fun (key, values) -> key, values |> List.map valueProjection |> Set.ofList)

    let groupByMapAndFold keyProjection valueProjection folder state source =
        source |> List.groupBy keyProjection |> List.map (
            fun (key, values) -> key, values |> List.map valueProjection |> List.fold folder state)

    let trySplitAtFirst predicate source = 
        match source |> List.tryFindIndex predicate with
        | Some idx -> Some (source |> List.splitAt idx)
        | None -> None

    let splitAtFirst predicate source =
        source |> trySplitAtFirst predicate |> Option.get

module Seq = 
    let groupByAndMap keyProjection valueProjection source =
        source |> Seq.groupBy keyProjection |> Seq.map (fun (key, values) -> key, values |> Seq.map valueProjection)

    let groupByAndMapIntoSet keyProjection valueProjection source =
        source |> Seq.groupBy keyProjection |> Seq.map (fun (key, values) -> key, values |> Seq.map valueProjection |> Set.ofSeq)

    let groupByMapAndFold keyProjection valueProjection folder state source =
        source |> Seq.groupBy keyProjection |> Seq.map (
            fun (key, values) -> key, values |> Seq.map valueProjection |> Seq.fold folder state)

module Array = 
    let inline swap i j (source : 'a array) = 
        let tmp = source[i]
        source[i] <- source[j]
        source[j] <- tmp

    let groupByAndMap keyProjection valueProjection source =
        source |> Array.groupBy keyProjection |> Array.map (fun (key, values) -> key, values |> Array.map valueProjection)

    let iterPermutations action source = 
        let rec _iter action arr i = 
            if i = Array.length arr - 1 then action (Array.copy arr)
            for j = i to Array.length arr - 1 do
                swap i j arr
                _iter action arr (i + 1)
                swap i j arr
        _iter action (Array.copy source) 0

    let binarySearchLessOrEqual value source =
        if Array.isEmpty source then None
        else (0, Array.length source - 1) ||> BinarySearch.lastLessOrEqual (fun i -> source[i] <= value)

    let binarySearchEqual value source = 
        match binarySearchLessOrEqual value source with
        | Some idx -> if source[idx] = value then Some idx else None
        | None -> None

    let insertSorted value source = 
        match binarySearchLessOrEqual value source with
        | Some idx -> source |> Array.insertAt (idx + 1) value
        | None -> source |> Array.insertAt 0 value

    let insertSortedUnique value source = 
        match binarySearchLessOrEqual value source with
        | Some idx -> if source[idx] = value then source else source |> Array.insertAt (idx + 1) value
        | None -> source |> Array.insertAt 0 value

module Map =
    let inline get key orDefault = 
        Map.tryFind key >> Option.defaultValue orDefault

    let addSeq seq map =
        (map, seq) ||> Seq.fold (fun map (k, v) -> map |> Map.add k v)

module ArrayList = 
    let inline makeRoom capacity (arr, len) =
        if Array.length arr >= capacity then (arr, len)
        else
        let newArr = max 1 (Array.length arr * 2) |> Array.zeroCreate
        Array.blit arr 0 newArr 0 len
        (newArr, len)

    let mutAppend value (arr, len) = 
        let arr, len = (arr, len) |> makeRoom (len + 1)
        arr[len] <- value
        (arr, len + 1)

    let mutAppendArr values (arr, len) = 
        let count = Array.length values
        let arr, len = (arr, len) |> makeRoom (len + count)
        Array.blit values 0 arr len count
        (arr, len + count)

namespace Utils

[<AutoOpen>]
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

    module JSON = 
        open System.Text.Json
        open System.Text.Json.Serialization

        type FormatOptions = { MaxInlineWidth : int }
        module FormatOptions = 
            let defaults = { MaxInlineWidth = 80 }

        type FormatResult = InlineString of string | IndentedLines of string
        module FormatResult = 
            let isInlineString result = match result with InlineString _ -> true | _ -> false
            let isIndentedLines result = match result with IndentedLines _ -> true | _ -> false
            let inlineString result = match result with InlineString str -> str | _ -> failwith "not an inline result"
            let indentedLines result = match result with IndentedLines lines -> lines | _ -> failwith "not an indented result"

        let private tryFormatResultsAsInlineArray options formatResults = 
            if not (formatResults |> Seq.forall FormatResult.isInlineString) then None
            else
            let elemsStr = formatResults |> Seq.map FormatResult.inlineString |> String.concat ", "
            let formatted = $"[{elemsStr}]"
            if String.length formatted <= options.MaxInlineWidth then Some formatted else None

        let private tryFormatResultsAsInlineObject options namesWithFormatResults = 
            if not (namesWithFormatResults |> Seq.map snd |> Seq.forall FormatResult.isInlineString) then None
            else
            let elemsStr = namesWithFormatResults |> Seq.map (fun (name, result) ->
                                $"{name}: {FormatResult.inlineString result}") |> String.concat ", "
            let formatted = $"{{ {elemsStr} }}"
            if String.length formatted <= options.MaxInlineWidth then Some formatted else None

        let rec private formatElement options level (element : JsonElement) = 
            let curIndentation = System.String(' ', level * 2)
            let nextIndentation = curIndentation + System.String(' ', 2)
            match element.ValueKind with 
            | JsonValueKind.Object ->
                let childResultsWithNames = element.EnumerateObject() |> Seq.map (
                    fun prop -> (prop.Name, prop.Value |> formatElement options (level + 1)))
                match childResultsWithNames |> tryFormatResultsAsInlineObject options with
                | Some str -> InlineString str
                | None ->
                    childResultsWithNames |> Seq.map (fun (name, result) -> 
                        match result with
                        | InlineString str -> $"{nextIndentation}{name}: {str}"
                        | IndentedLines lines -> 
                            if not (lines.StartsWith(nextIndentation)) then failwith $"unexpected indentation at {lines}"
                            let trimmedLines = lines.Substring(String.length nextIndentation)
                            $"{nextIndentation}{name}: {trimmedLines}"
                    ) |> String.concat ",\n" 
                    |> fun lines -> IndentedLines $"{curIndentation}{{\n{lines}\n{curIndentation}}}"
            | JsonValueKind.Array ->
                let childResults = element.EnumerateArray() |> Seq.map (formatElement options (level + 1))
                match childResults |> tryFormatResultsAsInlineArray options with
                | Some str -> InlineString str
                | None ->
                    childResults |> Seq.map (fun result -> 
                        match result with
                        | InlineString str -> nextIndentation + str
                        | IndentedLines lines -> lines
                    ) |> String.concat ",\n" 
                    |> fun lines -> IndentedLines $"{curIndentation}[\n{lines}\n{curIndentation}]"
            | JsonValueKind.String -> InlineString $"\"{element.GetString()}\""
            | JsonValueKind.Number | JsonValueKind.True | JsonValueKind.False -> InlineString $"{element.GetRawText()}"
            | JsonValueKind.Null -> InlineString "null"
            | _ -> failwith $"unexpected ValueKind {element.ValueKind}"

        let serializeToString formatOptions x = 
            // Using JsonFSharp to support distriminated unions. Add any .WithXXX() calls to customize the format
            let options = JsonFSharpOptions.Default().ToJsonSerializerOptions()
            match System.Text.Json.JsonSerializer.SerializeToElement(x, options) |> formatElement formatOptions 0 with
            | InlineString line -> line
            | IndentedLines lines -> lines

    let toInspectString x = JSON.serializeToString JSON.FormatOptions.defaults x
    let inspectWithLabel label x = printfn $"{label}: {toInspectString x}"; x
    let inspect x = inspectWithLabel "INSPECT" x

    let inspectSeq seq = 
        printfn "seq {"
        seq |> Seq.iter (fun x -> printfn $"    {x}")
        printfn "}"
        seq

    let printn x = printfn $"{x}"
    let printSeq seq = inspectSeq seq |> ignore

open Globals

module File = 
    let tryReadLinesWithLogging path = 
        try
            System.IO.File.ReadAllLines path |> Array.toList |> Some
        with
            | :? System.IO.FileNotFoundException -> printfn $"File not found: {path}"; None

module Math = 
    let inline gcd a b = 
        let rec loop a b =
            if b = makeZero () then abs a 
            else loop b (a % b)
        loop a b

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

    let inline contains x r = r.Start <= x && x <= r.Finish

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

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module Seq = 
    let groupByAndMap keyProjection valueProjection source =
        source |> Seq.groupBy keyProjection |> Seq.map (fun (key, values) -> key, values |> Seq.map valueProjection)

    let groupByAndMapIntoSet keyProjection valueProjection source =
        source |> Seq.groupBy keyProjection |> Seq.map (fun (key, values) -> key, values |> Seq.map valueProjection |> Set.ofSeq)

    let groupByMapAndFold keyProjection valueProjection folder state source =
        source |> Seq.groupBy keyProjection |> Seq.map (
            fun (key, values) -> key, values |> Seq.map valueProjection |> Seq.fold folder state)

[<RequireQualifiedAccess>]
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

[<RequireQualifiedAccess>]
module Map =
    let inline get key orDefault = 
        Map.tryFind key >> Option.defaultValue orDefault

    let addSeq seq map =
        (map, seq) ||> Seq.fold (fun map (k, v) -> map |> Map.add k v)

[<RequireQualifiedAccess>]
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

type Dict<'a, 'b> = System.Collections.Generic.Dictionary<'a, 'b>

[<RequireQualifiedAccess>]
module Dict = 
    let inline ofSeq seq = 
        seq |> Seq.map (fun (a, b) -> System.Collections.Generic.KeyValuePair(a, b)) |> Dict<_, _>

    let inline toSeq dict = 
        dict |> Seq.map (|KeyValue|)

    let inline ofMap map = 
        map |> Map.toSeq |> ofSeq

    let inline toMap dict = 
        dict |> Seq.map (|KeyValue|) |> Map.ofSeq

    let inline tryFind (source : Dict<_, _>) key = 
        match source.TryGetValue(key) with
        | true, value -> Some value
        | false, _ -> None

    let inline find source key =
        tryFind source key |> Option.get

    let inline containsKey source key = 
        tryFind source key |> Option.isSome

    let inline get source key orDefault = 
        match tryFind source key with
        | Some value -> value
        | None -> orDefault

    let inline add (source : Dict<_, _>) key value = 
        source.Add(key, value)

    let inline count (source : Dict<_, _>) =
        source.Count

module Memoize =
    let init () = Dict<_, _>()

    let private _run mem key fn = 
        match Dict.tryFind mem key with
        | Some value -> value
        | None ->
            let value = fn ()
            Dict.add mem key value
            value

    let run mem fn a = 
        _run mem a (fun () -> fn a)

    let run2 mem fn a b = 
        _run mem (a, b) (fun () -> fn a b)

    let run3 mem fn a b c = 
        _run mem (a, b, c) (fun () -> fn a b c)

    let run4 mem fn a b c d = 
        _run mem (a, b, c, d) (fun () -> fn a b c d)

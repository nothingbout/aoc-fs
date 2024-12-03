namespace Utils
open Utils.Globals

module Char = 
    let isDigit c = 
        c >= '0' && c <= '9'

    let isWhitespace c = 
        c = ' ' || c = '\t'

[<Struct>]
[<CustomEquality>]
[<NoComparison>]
type Substring = { Str : string; Range : int Range }
    with
        member inline a.CharAt i = a.Str[a.Range.Start + i] 

        override a.ToString (): string = a.Str.Substring(a.Range.Start, IntRange.length a.Range)
        override a.GetHashCode() = a.ToString().GetHashCode()
        interface System.IEquatable<Substring> with
            member a.Equals b =
                printn "Equals b called"
                let a = a
                let n = IntRange.length a.Range
                n = IntRange.length b.Range && seq { 0..n - 1 } |> Seq.forall (fun i -> a.CharAt i = b.CharAt i)
        override a.Equals obj =
            printn "Equals obj called"
            match obj with
            | :? Substring as b -> (a :> System.IEquatable<_>).Equals b
            | _ -> false

module Substring = 
    let inline make str range = { Str = str; Range = range }
    let inline ofString str = make str (IntRange.withLength 0 (String.length str))
    let toString substr = substr.ToString()

    let inline length { Range = range } = IntRange.length range
    let inline isEmpty substr = length substr = 0
    let inline charAt i (substr : Substring) = substr.CharAt i

    let inline sub from to' r = make r.Str (Range.sub from to' r.Range)
    let inline subFrom from r = make r.Str (Range.subFrom from r.Range)
    let inline subTo to' r = make r.Str (Range.subTo to' r.Range)

    let equal a b =
        length a = length b && seq { 0..length a - 1 } |> Seq.forall (fun i -> charAt i a = charAt i b)

    let startsWith prefix substr =
        let prefix = ofString prefix
        substr |> subTo (min (length substr - 1) (length prefix - 1)) |> equal prefix

    let endsWith suffix substr =
        let suffix = ofString suffix
        substr |> subFrom (max 0 (length substr - length suffix)) |> equal suffix

    let tryTrimPrefix prefix substr =
        if startsWith prefix substr then Some (subFrom (String.length prefix) substr)
        else None

    let tryTrimSuffix suffix substr =
        if endsWith suffix substr then Some (subTo (length substr - String.length suffix - 1) substr)
        else None

    let trimPrefix prefix str = 
        match tryTrimPrefix prefix str with
        | Some str -> str
        | None -> failwith $"expected to start with '{prefix}' but found '{str}'"        

    let trimSuffix suffix str = 
        match tryTrimSuffix suffix str with
        | Some str -> str
        | None -> failwith $"expected to end with '{suffix}' but found '{str}'"

    let tryFindIndex (pred : char -> bool) substr = 
        seq { 0..length substr - 1 } |> Seq.tryFindIndex (fun i -> charAt i substr |> pred)

module String = 
    let ofSeq (source : char seq) = source |> Array.ofSeq |> System.String
    let ofList (source : char list) = source |> Array.ofList |> System.String
    let ofArray (source : char array) = source |> System.String
    let toArray (source : string) = source.ToCharArray()

    let toLower (str : string) = str.ToLower()
    let toUpper (str : string) = str.ToUpper()
    let sub (from : int) (to' : int) (str : string) = str.Substring(from, to')
    let subFrom (from : int) (str : string) = str.Substring(from)
    let subTo (to' : int) (str : string) = str.Substring(0, to' + 1)

    let isEmpty str = String.length str = 0
    let startsWith (prefix : string) (str : string) = str.StartsWith(prefix)
    let endsWith (suffix : string) (str : string) = str.EndsWith(suffix)

    let tryTrimPrefix (prefix : string) (str : string) = 
        if startsWith prefix str
        then Some <| subFrom (String.length prefix) str
        else None

    let trimPrefix prefix str = 
        match tryTrimPrefix prefix str with
        | Some str -> str
        | None -> failwith $"expected to start with '{prefix}' but found '{str}'"

    let tryTrimSuffix (suffix : string) (str : string) = 
        if endsWith suffix str
        then Some <| subTo (String.length str - String.length suffix - 1) str
        else None

    let trimSuffix (suffix : string) (str : string) = 
        match tryTrimSuffix suffix str with
        | Some str -> str
        | None -> failwith $"expected to end with '{suffix}' but found '{str}'"

    let splitBy (pred : char -> bool) (str : string) : string list = 
        let words, cur = (([], []), str) ||> Seq.fold (fun (words, cur) c -> 
            if pred c then ((cur |> List.rev |> ofList) :: words, []) else (words, c :: cur))
        (cur |> List.rev |> ofList) :: words |> List.rev

    let splitByAndRemoveEmpty pred str =
        str |> splitBy pred |> List.filter (isEmpty >> not)

    let splitByWhitespace str = 
        str |> splitByAndRemoveEmpty Char.isWhitespace

    let splitByString (pattern : string) (str : string) = 
        str.Split(pattern) |> List.ofArray

    let splitByStrings (patterns : string list) (str : string) = 
        str.Split(Array.ofList patterns, System.StringSplitOptions.None) |> List.ofArray

    let extractGroupsBy (filter : char -> bool) str = 
        str |> splitByAndRemoveEmpty (filter >> not)

    let extractInts str = 
        str |> extractGroupsBy Char.isDigit |> List.map int

    let tryFindIndexOfString (pattern : string) (str : string) = 
        match str.IndexOf(pattern) with
        | idx when idx >= 0 -> Some idx
        | -1 -> None
        | idx -> failwith $"unexpected idx {idx}"

    let replaceOccurencesOfString (pattern : string) (replaceWith : string) (str : string) =
        str.Replace(pattern, replaceWith)

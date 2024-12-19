namespace Utils

module Char = 
    let isDigit c = 
        c >= '0' && c <= '9'

    let isWhitespace c = 
        c = ' ' || c = '\t'

[<Struct>][<CustomEquality>][<NoComparison>]
type Substr = { Str : string; Range : int Range }

    with
        member inline a.CharAt i = a.Str[a.Range.Start + i] 

        override a.ToString (): string = a.Str.Substring(a.Range.Start, IntRange.length a.Range)
        override a.GetHashCode() = a.ToString().GetHashCode()
        interface System.IEquatable<Substr> with
            member a.Equals b =
                IntRange.length a.Range = IntRange.length b.Range &&
                System.String.Compare(a.Str, a.Range.Start, b.Str, b.Range.Start, IntRange.length a.Range) = 0
        override a.Equals obj =
            match obj with
            | :? Substr as b -> (a :> System.IEquatable<_>).Equals b
            | _ -> false

module Substr = 
    let inline make str range = { Str = str; Range = range }
    let inline ofStr str = make str (IntRange.withLength 0 (String.length str))
    let toStr substr = substr.ToString()

    let inline length { Range = range } = IntRange.length range
    let inline isEmpty substr = length substr = 0
    let inline charAt i (substr : Substr) = substr.CharAt i

    let inline sub from to' r = make r.Str (Range.sub from to' r.Range)
    let inline subFrom from r = make r.Str (Range.subFrom from r.Range)
    let inline subTo to' r = make r.Str (Range.subTo to' r.Range)

    let equal (a : Substr) (b : Substr) =
        IntRange.length a.Range = IntRange.length b.Range &&
        System.String.Compare(a.Str, a.Range.Start, b.Str, b.Range.Start, IntRange.length a.Range) = 0

    let startsWith prefix substr =
        let prefix = ofStr prefix
        substr |> subTo (min (length substr - 1) (length prefix - 1)) |> equal prefix

    let endsWith suffix substr =
        let suffix = ofStr suffix
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
    let isEmpty str = String.length str = 0

    let toLower (str : string) = str.ToLower()
    let toUpper (str : string) = str.ToUpper()
    let sub from to' =          Substr.ofStr >> Substr.sub from to'         >> Substr.toStr
    let subFrom from =          Substr.ofStr >> Substr.subFrom from         >> Substr.toStr
    let subTo to' =             Substr.ofStr >> Substr.subTo to'            >> Substr.toStr
    let startsWith prefix =     Substr.ofStr >> Substr.startsWith prefix
    let endsWith suffix =       Substr.ofStr >> Substr.endsWith suffix
    let tryTrimPrefix prefix =  Substr.ofStr >> Substr.tryTrimPrefix prefix >> Option.map Substr.toStr
    let tryTrimSuffix suffix =  Substr.ofStr >> Substr.tryTrimSuffix suffix >> Option.map Substr.toStr
    let trimPrefix prefix =     Substr.ofStr >> Substr.trimPrefix prefix    >> Substr.toStr
    let trimSuffix suffix =     Substr.ofStr >> Substr.trimSuffix suffix    >> Substr.toStr

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

    let extractDigitGroups str = 
        str |> extractGroupsBy Char.isDigit

    let extractInts str = 
        str |> extractDigitGroups |> List.map int

    let tryFindIndexOfString (pattern : string) (str : string) = 
        match str.IndexOf(pattern) with
        | idx when idx >= 0 -> Some idx
        | -1 -> None
        | idx -> failwith $"unexpected idx {idx}"

    let containsString (pattern : string) = tryFindIndexOfString pattern >> Option.isSome

    let replaceOccurencesOfString (pattern : string) (replaceWith : string) (str : string) =
        str.Replace(pattern, replaceWith)

namespace Utils

module Str = 
    let ofSeq (source : char seq) = source |> Array.ofSeq |> System.String
    let ofList (source : char list) = source |> Array.ofList |> System.String
    let ofArray (source : char array) = source |> System.String
    let toArray (source : string) = source.ToCharArray()

    let toLower (str : string) = str.ToLower()
    let toUpper (str : string) = str.ToUpper()
    let sub (from : int) (to' : int) (str : string) = str.Substring(from, to')
    let subFrom (from : int) (str : string) = str.Substring(from)
    let subTo (to' : int) (str : string) = str.Substring(0, to')

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
        then Some <| subTo (String.length str - String.length suffix) str
        else None

    let trimSuffix (suffix : string) (str : string) = 
        match tryTrimSuffix suffix str with
        | Some str -> str
        | None -> failwith $"expected to end with '{suffix}' but found '{str}'"

    let splitByString (pattern : string) (str : string) = 
        str.Split(pattern) |> List.ofArray

    let splitByStrings (patterns : string list) (str : string) = 
        str.Split(Array.ofList patterns, System.StringSplitOptions.None) |> List.ofArray

    let tryFindIndexOfString (pattern : string) (str : string) = 
        match str.IndexOf(pattern) with
        | idx when idx >= 0 -> Some idx
        | -1 -> None
        | idx -> failwith $"unexpected idx {idx}"

    let replaceOccurencesOfString (pattern : string) (replaceWith : string) (str : string) =
        str.Replace(pattern, replaceWith)

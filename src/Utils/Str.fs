namespace Utils

module Str = 
    let ofSeq (source : char seq) = source |> Array.ofSeq |> System.String
    let ofList (source : char list) = source |> Array.ofList |> System.String
    let ofArray (source : char array) = source |> System.String
    let toArray (source : string) = source.ToCharArray()

    let sub (from : int) (to' : int) (str : string) = str.Substring(from, to')
    let subFrom (from : int) (str : string) = str.Substring(from)
    let subTo (to' : int) (str : string) = str.Substring(0, to')

    let startsWith (prefix : string) (str : string) = str.StartsWith(prefix)
    let endsWith (suffix : string) (str : string) = str.EndsWith(suffix)

    let trimPrefix (prefix : string) (str : string) = 
        if str |> startsWith prefix
        then str |> subFrom (String.length prefix)
        else failwith $"expected to start with '{prefix}' but found '{str}'"

    let trimSuffix (suffix : string) (str : string) = 
        if str |> endsWith suffix
        then str |> subTo (String.length str - String.length suffix)
        else failwith $"expected to end with '{suffix}' but found '{str}'"

    let splitByString (pattern : string) (str : string) = 
        str.Split(pattern) |> List.ofArray

    let splitByStrings (patterns : string list) (str : string) = 
        str.Split(Array.ofList patterns, System.StringSplitOptions.None) |> List.ofArray

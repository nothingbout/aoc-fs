namespace Utils
open Utils.Globals

type ScanResult<'T> =
  | ScanSuccess of 'T
  | ScanError of string

type ScanFunc<'T> = Substring -> Substring * ScanResult<'T>

module ScanFunc = 
    let return' value : ScanFunc<'T> = 
        fun substr -> substr, (ScanSuccess value)

    let returnFrom value : ScanFunc<'T> = 
        value

    let bind (curFunc : ScanFunc<'T>) (next : 'T -> ScanFunc<'U>) : ScanFunc<'U> = 
        fun curSubstr ->
            let nextSubstr, result = curFunc curSubstr
            match result with
            | ScanSuccess value -> 
                let nextFunc = next value
                nextFunc nextSubstr
            | ScanError err -> 
                nextSubstr, ScanError err

    // let yield' (next : unit -> ScanFunc<'T>) : ScanFunc<'T> = 
    //     fun nextSubstr ->
    //         let nextFunc = next ()
    //         nextFunc nextSubstr

type ScanFuncBuilder() = 
    member _.Return(value) = ScanFunc.return' value
    member _.ReturnFrom(value) = ScanFunc.returnFrom value
    member _.Bind(scanner, next) = ScanFunc.bind scanner next
    // member _.Yield(next) = ScanFunc.yield' next

module Scan = 
    let scan = ScanFuncBuilder()

    let finish (_substr, result) : 'T =
        match result with
        | ScanSuccess value -> value
        | ScanError err -> failwith err

    let maybe (substr, result) =
        match result with
        | ScanSuccess value -> substr, ScanSuccess (Some value)
        | ScanError _ -> substr, ScanSuccess None
    
    // All functions below are Substring -> Substring * ScanResult<'T>
    let inline error err substr = 
        substr, 
        ScanError err

    let inline success value substr = 
        substr, 
        ScanSuccess value

    let takeAll substr = 
        Substring.ofString "" |> success (substr |> Substring.toString)

    let skip n substr =
        if n <= Substring.length substr 
        then substr |> Substring.subFrom n |> success ()
        else substr |> error "Scan.skip input substring not long enough"

    let take n substr = 
        if n <= Substring.length substr
        then substr |> Substring.subFrom n |> success (substr |> Substring.subTo (n - 1) |> Substring.toString)
        else substr |> error "Scan.skip input substring not long enough"

    let rec next (scanFunc : ScanFunc<'a>) substr =
        match substr |> scanFunc with
        | substr, ScanSuccess value -> substr |> success value
        | _, ScanError _ ->
            match substr |> skip 1 with
            | substr, ScanSuccess _ -> substr |> next scanFunc
            | _, ScanError _ -> substr |> error "Scan.next did not find match"

    let skipString pattern substr = 
        if substr |> Substring.startsWith pattern 
        then substr |> Substring.subFrom (String.length pattern) |> success ()
        else substr |> error $"Scan.skipString expected to start with '{pattern}' but found '{substr}'"

    let skipWhile (pred : char -> bool) substr =
        match substr |> Substring.tryFindIndex (pred >> not) with
        | None -> substr |> success ()
        | Some idx -> substr |> skip idx

    let takeWhile (pred : char -> bool) substr = 
        match substr |> Substring.tryFindIndex (pred >> not) with
        | None -> substr |> takeAll
        | Some idx -> substr |> take idx

    let takeWhileNonEmpty (pred : char -> bool) substr = 
        match substr |> Substring.tryFindIndex (pred >> not) with
        | None -> substr |> takeAll
        | Some 0 -> substr |> error "Scan.chars zero chars matched"
        | Some idx -> substr |> take idx

    let skipSpaces = skipWhile (fun c -> c = ' ')

    let takeDigits substr = 
        substr |> takeWhileNonEmpty Char.isDigit

    let takePositiveInt substr =
        substr |> scan {
            let! str = takeDigits
            return (str |> toString |> int)
        }

    let takeInt substr =
        substr |> scan {
            let! neg = skipString "-" >> maybe
            let! str = takeWhileNonEmpty (fun c -> Char.isDigit c)
            let posInt = str |> toString |> int
            return if Option.isSome neg then -posInt else posInt
        }

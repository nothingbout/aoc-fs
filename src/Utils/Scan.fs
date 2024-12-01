namespace Utils

type ScanResult<'T> =
  | ScanSuccess of 'T
  | ScanError of string

type ScanSeq = list<char>
module ScanSeq = 
    let ofString (str : string) : ScanSeq = str |> List.ofSeq
    let string seq = seq |> Array.ofList |> System.String

type ScanFunc<'T> = ScanSeq -> ScanSeq * ScanResult<'T>

module ScanFunc = 
    let return' value : ScanFunc<'T> = 
        fun scanSeq -> scanSeq, (ScanSuccess value)

    let bind (curFunc : ScanFunc<'T>) (next : 'T -> ScanFunc<'U>) : ScanFunc<'U> = 
        fun curSeq ->
            let nextSeq, result = curFunc curSeq
            match result with
            | ScanSuccess value -> 
                let nextFunc = next value
                nextFunc nextSeq
            | ScanError err -> 
                nextSeq, ScanError err

    // let yield' (next : unit -> ScanFunc<'T>) : ScanFunc<'T> = 
    //     fun nextSeq ->
    //         let nextFunc = next ()
    //         nextFunc nextSeq

type ScanFuncBuilder() = 
    member _.Return(value) = ScanFunc.return' value
    member _.Bind(scanner, next) = ScanFunc.bind scanner next
    // member _.Yield(next) = ScanFunc.yield' next

module Scan = 
    let scan = ScanFuncBuilder()

    let finish (_seq, result) : 'T =
        match result with
        | ScanSuccess value -> value
        | ScanError err -> failwith err

    let tryFinish (_seq, result) : 'T option =
        match result with
        | ScanSuccess value -> Some value
        | ScanError err -> None

    // All functions are ScanSeq -> ScanSeq * ScanResult<'T>
    let error err seq = 
        seq, 
        ScanError err

    let success value seq = 
        seq, 
        ScanSuccess value

    let all seq = 
        List.empty,
        seq |> ScanSeq.string |> ScanSuccess

    let take n pos = 
        pos |> List.skip n, 
        pos |> List.take n |> ScanSeq.string |> ScanSuccess

    let skip n pos = 
        pos |> List.skip n, 
        ScanSuccess ()

    let rec private _trySkipString (pattern : ScanSeq) (seq : ScanSeq) : ScanSeq option = 
        match pattern, seq with 
        | [], _ -> Some seq
        | _, [] -> None
        | x :: xs, y :: ys when x = y -> _trySkipString xs ys
        | _ -> None

    let skipString (pattern : string) (seq : ScanSeq) : ScanSeq * ScanResult<unit> = 
        match seq |> _trySkipString (ScanSeq.ofString pattern) with
        | Some seq -> seq, ScanSuccess ()
        | None -> seq |> error $"Scan.skipString expected to start with '{pattern}' but found '{ScanSeq.string seq}'"

    let trySkipString (pattern : string) (seq : ScanSeq) : ScanSeq * ScanResult<bool> = 
        match skipString pattern seq with
        | seq, ScanSuccess _ -> seq, ScanSuccess true
        | seq, ScanError _ -> seq, ScanSuccess false

    let charsOrEmpty (pred : char -> bool) (seq : ScanSeq) : ScanSeq * ScanResult<string> = 
        match seq |> List.tryFindIndex (pred >> not) with
        | None -> seq |> all
        | Some idx -> seq |> take idx

    let skipSpaces seq = 
        let (seq, _) = seq |> charsOrEmpty (fun c -> c = ' ')
        seq |> success ()

    let chars pred seq = 
        match charsOrEmpty pred seq with
        | seq, ScanError err -> seq |> error err
        | seq, ScanSuccess str when String.length str = 0 -> seq |> error "Scan.chars zero chars matched"
        | seq, ScanSuccess str -> seq |> success str

    let digits seq = 
        seq |> chars Char.isDigit

    let positiveInt seq =
        seq |> scan {
            let! str = digits
            return (int str)
        }

    let int seq =
        seq |> scan {
            let! str = chars (fun c -> Char.isDigit c || c = '-')
            return (int str)
        }

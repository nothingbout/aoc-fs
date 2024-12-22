module AOC2024.Day22
open Utils

let parseNumber line = 
    line |> int64

let mix value secret = 
    secret ^^^ value

let prune secret = 
    secret %+ 16777216L

let processSecret secret = 
    let secret = secret * 64L |> mix secret |> prune
    let secret = secret / 32L |> mix secret |> prune
    let secret = secret * 2048L |> mix secret |> prune
    secret

let rec getSecrets n secrets initial = 
    match n with
    | 0 -> List.rev (initial :: secrets)
    | _ -> getSecrets (n - 1) (initial :: secrets) (processSecret initial)

let getPrices secrets = 
    secrets |> List.map (fun a -> a % 10L |> int)

let getDeltas secrets = 
    secrets |> List.pairwise |> List.map (fun (a, b) -> b - a)

let arrKey d1 d2 d3 d4 = 
    let k = (d1 + 20)
    let k = k * 40 + (d2 + 20)
    let k = k * 40 + (d3 + 20)
    let k = k * 40 + (d4 + 20)
    k

let getProfitsByDeltas prices deltas = 
    let rec loop prices deltas (profits : int array) (keys : int list) = 
        match prices, deltas with
        | price :: prices, d1 :: d2 :: d3 :: d4 :: _ ->
            let deltas = List.skip 1 deltas
            let k = arrKey d1 d2 d3 d4
            if profits[k] = -1000 then 
                profits[k] <- price 
                loop prices deltas profits (k :: keys)
            else
                loop prices deltas profits keys
        | _ -> keys, profits
    loop (List.skip 4 prices) deltas (Array.init (40 * 40 * 40 * 40) (fun _ -> -1000)) List.empty

let solveP1 (inputLines: string list) = 
    inputLines |> List.map parseNumber
    |> List.map (getSecrets 2000 [] >> List.last) 
    |> List.sum |> Answer.int64
    
let solveP2 (inputLines: string list) = 
    let initialSecrets = inputLines |> List.map parseNumber

    let profitsForAll =
        initialSecrets |> Array.ofList |> Array.Parallel.map (fun initial ->
            let prices = getSecrets 2000 [] initial |> getPrices
            let deltas = prices |> getDeltas
            getProfitsByDeltas prices deltas
        )

    let keys = profitsForAll |> Array.map (fst >> Array.ofList) |> Array.concat |> Array.distinct
    keys |> Array.Parallel.map (fun key -> 
        profitsForAll |> Array.sumBy (fun (_, prices) -> 
            match prices[key] with
            | price when price > -10 -> price
            | _ -> 0
        )
    ) |> Seq.max |> Answer.int

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example.txt" (Answer.int64 37327623)
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.int64 18525593556L)
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.int 23)
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.int 2089)
    ]

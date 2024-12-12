module AOC2018.Day13
open Utils
open Utils.Globals

[<Struct>]
type Cart = { Pos : Vec2<int>; Dir : char; IntersectionIdx : int }

let parseMap lines = 
    let pieces = Array2.ofStringLines lines
    let mutable carts = []
    pieces |> Array2.iteri (fun pos c ->
        match c with
        | '<' | '>' -> 
            pieces[pos] <- '-'
            carts <- { Pos = pos; Dir = c; IntersectionIdx = 0 } :: carts
        | 'v' | '^' -> 
            pieces[pos] <- '|'
            carts <- { Pos = pos; Dir = c; IntersectionIdx = 0 } :: carts
        | _ -> ()
    )
    (pieces, carts)

let printMap (pieces : Array2<char>) carts = 
    let map = Array2.copy pieces
    for cart in carts do
        map[cart.Pos] <- cart.Dir
    // for y = 0 to Array2.height map - 1 do
    //     map |> Array2.row y |> Str.ofArray |> printn

    map |> Array2.toStringLines |> Array.iter printn

    printfn ""

let directionToVec dir = 
    match dir with '>' -> Vec2.make 1 0 | '^' -> Vec2.make 0 -1 | '<' -> Vec2.make -1 0 | 'v' -> Vec2.make 0 1 | _ -> failwith $"unexpected direction {dir}"

let turnLeft dir = 
    match dir with '>' -> '^' | '^' -> '<' | '<' -> 'v' | 'v' -> '>' | _ -> failwith $"unexpected direction {dir}"

let turnRight dir = 
    match dir with '>' -> 'v' | 'v' -> '<' | '<' -> '^' | '^' -> '>' | _ -> failwith $"unexpected direction {dir}"

let moveCart (pieces : Array2<char>) cart =
    let pos = cart.Pos + directionToVec cart.Dir
    match pieces[pos] with
    | '-' | '|' -> { cart with Pos = pos }
    | '+' ->
        match cart.IntersectionIdx with
        | 0 -> { Pos = pos; Dir = turnLeft cart.Dir; IntersectionIdx = 1 }
        | 1 -> { Pos = pos; Dir = cart.Dir; IntersectionIdx = 2 }
        | 2 -> { Pos = pos; Dir = turnRight cart.Dir; IntersectionIdx = 0 }
        | _ -> failwith $"unexpected cart intersection idx {cart.IntersectionIdx}"
    | '/' ->
        let dir = 
            match cart.Dir with
            | '>' | '<' -> turnLeft cart.Dir
            | '^' | 'v' -> turnRight cart.Dir
            | _ -> failwith $"unexpected cart dir {cart.Dir}"
        { cart with Pos = pos; Dir = dir }
    | '\\' ->
        let dir = 
            match cart.Dir with
            | '>' | '<' -> turnRight cart.Dir
            | '^' | 'v' -> turnLeft cart.Dir
            | _ -> failwith $"unexpected cart dir {cart.Dir}"
        { cart with Pos = pos; Dir = dir }
    | c -> failwith $"unexpected track piece {c}"

let tryFindCrash others cart = 
    match others |> List.tryFindIndex (fun other -> other.Pos = cart.Pos) with
    | None -> None
    | Some _ -> Some cart.Pos

let removeCartsAtPos pos carts = 
    carts |> List.filter (fun cart -> cart.Pos <> pos)

let rec moveCarts pieces crashes moved remaining = 
    match remaining with
    | [] -> (moved, crashes)
    | cart :: remaining -> 
        let cart = cart |> moveCart pieces
        match cart |> tryFindCrash moved with
        | Some pos -> moveCarts pieces (pos :: crashes) (moved |> removeCartsAtPos pos) remaining
        | None -> 
        match cart |> tryFindCrash remaining with
        | Some pos -> moveCarts pieces (pos :: crashes) moved (remaining |> removeCartsAtPos pos)
        | None -> 
        moveCarts pieces crashes (cart :: moved) remaining
        
let rec tickUntilCrash pieces carts =
    // printMap pieces carts
    let newCarts, crashes = carts |> List.sortBy (fun cart -> (cart.Pos.Y, cart.Pos.X)) |> moveCarts pieces [] []
    match crashes with
    | pos :: _ -> pos
    | [] -> tickUntilCrash pieces newCarts

let rec tickUntilOneRemaining pieces carts =
    // printMap pieces carts
    match carts with 
    | [cart] -> cart.Pos
    | _ -> 
    let newCarts, _ = carts |> List.sortBy (fun cart -> (cart.Pos.Y, cart.Pos.X)) |> moveCarts pieces [] []
    tickUntilOneRemaining pieces newCarts

let solveP1 (inputLines: string list) = 
    let pieces, carts = parseMap inputLines
    let crashPos = tickUntilCrash pieces carts
    sprintf $"{crashPos.X},{crashPos.Y}" |> Answer.string
    
let solveP2 (inputLines: string list) = 
    let pieces, carts = parseMap inputLines
    let onePos = tickUntilOneRemaining pieces carts
    sprintf $"{onePos.X},{onePos.Y}" |> Answer.string

let getPuzzles () = 
    [
        Puzzle.create solveP1 "Part 1" "example1.txt" (Answer.string "7,3")
        Puzzle.create solveP1 "Part 1" "input.txt" (Answer.string "45,34")
        Puzzle.create solveP2 "Part 2" "example2.txt" (Answer.string "6,4")
        Puzzle.create solveP2 "Part 2" "input.txt" (Answer.string "91,25")
    ]

module Utils.Queue

[<Struct>]
type Queue<'a> = { In : 'a list; Out : 'a list }

module Queue = 
    let inline enqueue value q = 
        { In = value :: q.In; Out = q.Out }

    let tryDequeue q = 
        match q.Out with
        | value :: out -> Some (value, { In = q.In; Out = out })
        | [] -> 
        match List.rev q.In with
        | value :: out -> Some (value, { In = []; Out = out })
        | [] -> None

    let dequeue q = 
        match tryDequeue q with
        | Some p -> p
        | None -> failwith "empty queue"

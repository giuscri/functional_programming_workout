module Queue

type PaulsonRecord<'a> = { front: list<'a>; rear: list<'a> }
type Queue<'a> = Q of PaulsonRecord<'a>
exception EmptyQueue

let empty = Q { front=[]; rear=[] }
let isEmpty q = q = empty

let put value (Q r) =
        Q { r with rear = value::r.rear}

let rec get (Q r) =
    match r with
        |{ front=[]; rear=[] } -> raise EmptyQueue
        |{ front=x::xs; rear=[] } -> (x, Q { r with front=xs })
        |_ -> get (Q { front=r.front @ (List.rev r.rear); rear=[] })

let rec toList (Q r) =
    match r with
        |{ rear=[] } -> r.front
        |_ -> toList (Q { front=r.front @ (List.rev r.rear); rear=[] })

let ofList lst = Q { front=lst; rear=[] }

let rec put_list lst q =
    match lst with
        |[] -> q
        |x::xs -> put_list xs (put x q)

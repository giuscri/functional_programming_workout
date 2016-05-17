module Queue

type Queue<'a> = Q of list<'a>
exception EmptyQueue

let empty = Q []
let isEmpty q = q = empty

let put value (Q lst) = Q (lst @ [value])

let get (Q lst) =
    match lst with
        |[] -> raise EmptyQueue
        |head::tail -> (head, Q tail)

let toList (Q lst) = lst
let ofList lst = Q lst

let put_list lst (Q l) = Q (l @ lst)

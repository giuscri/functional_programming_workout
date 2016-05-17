module Stack

type Stack<'a> = S of list<'a>
exception EmptyStack

let empty = S []

let push el (S lst) = S (el::lst)

let pop (S lst) =
    match lst with
        |[] -> raise EmptyStack
        |x::xs -> (x, S xs)

let top (S lst) =
    match lst with
        |[] -> raise EmptyStack
        |x::_ -> x

let size (S lst) = List.length lst

#r "Stack"
open Stack

type operator =
    | Add
    | Prod
    | Minus

type token = 
    | Op of operator
    | C of int

let rec eval lst s =
    match lst with
        |[] -> let (C r, _) = pop s in r
        |x::xs ->
            match x with
                |C a -> eval xs (push x s)
                |Op Add ->
                    let (C b, s') = pop s
                    let (C a, s'') = pop s'
                    eval xs (push (C (a + b)) s'')
                |Op Prod ->
                    let (C b, s') = pop s
                    let (C a, s'') = pop s'
                    eval xs (push (C (a * b)) s'')
                |Op Minus ->
                    let (C b, s') = pop s
                    let (C a, s'') = pop s'
                    eval xs (push (C (a - b)) s'')

let evalRpn lst = eval lst empty

evalRpn [
    C 10
    C 6
    C 1
    Op Minus
    Op Prod
    C 4
    Op Minus
    C 2
    C 5
    Op Prod
    Op Add
] // Expected result is 56

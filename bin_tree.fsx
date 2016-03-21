type 'a BinTree = Null | Node of 'a * 'a BinTree * 'a BinTree;;

let T2 = Node (2, Null, Node (4, Null, Null));; 
let T7 = Node (7, Null, Node (10, Null, Node (13, Null, Null))) ;; 
let T8 = Node (8, Node (11, Null, Null), Null);; 
let T5 = Node (5, T7, T8);;
let T9 = Node (9, Null, Node (12, Null, Null));;
let T6 = Node (6, T9, Null);;
let T3 = Node (3, T5, T6);;
let T1 = Node (1, T2, T3);; 

let rec cast_to_float T =
    match T with
        |Null ->
            Null
        |Node (x, Null, Null) ->
            Node (x |> float, Null, Null)
        |Node (x, lsub, rsub) ->
            Node (x |> float, lsub |> cast_to_float, rsub |> cast_to_float);;

let rec in_order_visit T =
    match T with
        |Null -> []
        |Node (x, lsub, rsub) -> (lsub |> in_order_visit) @ [x] @ (rsub |> in_order_visit);;

let rec pre_order_visit T =
    match T with
        |Null -> []
        |Node (x, lsub, rsub) -> [x] @ (lsub |> pre_order_visit) @ (rsub |> pre_order_visit);;

let pp0 (T: int BinTree) =
    let lst0 = T |> pre_order_visit |> List.sort in
    let lst1 = T |> in_order_visit |> List.sort in
    lst0 = lst1;;

#r @"FsCheck";;
open FsCheck;;

do Check.Quick pp0;;

let rec search_element T e =
    match T with
        |Null ->    
            false
        |Node (x, lsub, rsub) when x=e ->
            true
        |Node (x, lsub, rsub) ->
            e |> search_element lsub || e |> search_element rsub;;

let rec is_member lst x =
    match lst with
        |[] -> false
        |head :: tail when head=x -> true
        |head :: tail -> x |> is_member tail;;

let pp1 (T: int BinTree, e: int) = 
    let lst = T |> in_order_visit in
    (e |> is_member lst) = (e |> search_element T);;

do Check.Quick pp1;;

let _in_order_visit T cb =
    let lst = in_order_visit T in
    let rec fn lst cb =
        match lst with
            |[] -> []
            |head :: tail when head |> cb -> head :: (cb |> fn tail)
            |head :: tail -> cb |> fn tail in
    fn lst cb;;

let even x = x % 2 = 0;;
let odd x = x % 2 <> 0;;

let count_internals_leaves T =
    let rec fn T (n_nodes, n_leaves) =
        match T with
            |Null -> (n_nodes, n_leaves)
            |Node (x, Null, Null) -> (n_nodes + 1, n_leaves + 1)
            |Node (x, lsub, rsub) ->
                (n_nodes + 1, n_leaves) |> fn lsub |> fn rsub in
    (0, 0) |> fn T;;

let filter_by_depth T n =
    let rec fn T n d =
        match T with
            |Null -> []
            |Node (x, _, _) when d=n -> [x]
            |Node (x, lsub, rsub) ->
                fn lsub n (d+1) @ fn rsub n (d+1) in
    fn T n 0;;

type direction = L | R;;

let rec fetch_element T lst =
    match (T, lst) with
        |(Null, _) -> None
        |(Node (x, _, _), []) -> Some x
        |(Node (x, lsub, rsub), head :: tail) when head=L ->
            tail |> fetch_element lsub
        |(Node (x, lsub, rsub), head :: tail) ->
            tail |> fetch_element rsub;;

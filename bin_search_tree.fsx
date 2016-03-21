type 'a BinSTree = Null | Node of 'a * 'a BinSTree * 'a BinSTree;;

let rec insert_element T x =
    match T with
        |Null -> Node (x, Null, Null)
        |Node (e, _, _) when x = e -> T
        |Node (e, lsub, rsub) when x < e ->
            Node (e, x |> insert_element lsub, rsub)
        |Node (e, lsub, rsub) ->
            Node (e, lsub, x |> insert_element rsub);;

let rec insert_list T lst =
    match lst with
        |[] -> T
        |head :: tail ->
            tail |> insert_list (head |> insert_element T);;

let int_list = [20; 10; 60; 15; 40; 100; 30; 50; 70; 35; 42; 58; 75; 32; 37];;

let int_tree = int_list |> insert_list Null;;

let str_list0 = ["pesca"; "banana"; "uva"; "albicocca"; "nocciola"; "ribes"];;

let str_tree0 = str_list0 |> insert_list Null;;

let str_list1 = ["limone"; "ciliegia"; "mela"; "pera"; "noce"];;

let str_tree1 = str_list1 |> insert_list str_tree0;;

let rec search_element T x =
    match T with
        |Null -> false
        |Node (e, _, _) when x = e -> true
        |Node (e, lsub, rsub) when x < e ->
            x |> search_element lsub
        |Node (e, lsub, rsub) ->
            x |> search_element rsub;;

let rec compute_search_path T x =
    match (T, x |> search_element T) with
        |(_, false) -> []
        |(Null, _) -> []
        |(Node (e, _, _), _) when x = e -> [e]
        |(Node (e, lsub, rsub), _) when x < e ->
            e :: (x |> compute_search_path lsub)
        |(Node (e, lsub, rsub), _) ->
            e :: (x |> compute_search_path rsub);;

let rec fetch_min T =
    match T with
        |Null -> None
        |Node (e, Null, _) -> Some e
        |Node (_, lsub, _) -> lsub |> fetch_min;;

let rec fetch_subtree T x =
    match T with
        |Null -> Null
        |Node (e, _, _) when x = e -> T
        |Node (e, lsub, _) when x < e -> x |> fetch_subtree lsub
        |Node (e, _, rsub) -> x |> fetch_subtree rsub;;
            

#r @"FsCheck";;
open FsCheck;;

let rev lst =
    let rec fn (lst, r) =
        match lst with
            |[] -> r
            |head :: tail -> (tail, head :: r) |> fn in
                (lst, []) |> fn;;

let append (lst0, lst1) =
    let _lst0 = rev lst0 in
    let rec fn (_lst0, lst1) =
        match lst1 with
            |[] -> _lst0
            |head :: tail -> fn (head :: _lst0, tail) in
    rev (fn (_lst0, lst1));;

let property_revrev_is_id (lst:int list) = lst |> rev |> rev = lst;;

let property_rev_is_id (lst:int list) = lst |> rev = lst;;

let property_append_has_identity (lst:int list) =
    (lst, []) |> append = lst && ([], lst) |> append = lst;;

let property_append_has_associativity (lst0:int list, lst1:int list, lst2:int list) =
    append (append (lst0, lst1), lst2) = append (lst0, append(lst1, lst2));;

let property_append_has_commutativity (lst0:int list, lst1:int list) =
    append (lst0, lst1) = append (lst1, lst0);;

printfn "*** property_revrev_is_id";;
do Check.Quick property_revrev_is_id;;

printfn "*** property_append_has_identity";;
do Check.Quick property_append_has_identity;;

printfn "*** property_append_has_associativity";;
do Check.Quick property_append_has_associativity;;

let property_rev_append_is_append_rev_rev (lst0:int list, lst1:int list) =
    rev (append (lst0, lst1)) = append (rev lst1, rev lst0)

printfn "*** property_rev_append_is_append_rev_rev";;
do Check.Quick property_rev_append_is_append_rev_rev;;

let rec is_ordered_list lst =
    match lst with
        |[] -> true
        |[element] -> true
        |head :: (middle :: tail) when head <= middle -> (middle :: tail) |> is_ordered_list
        |_ -> false;;

let rec insert_into_ordered_list (lst, element) =
    match lst with
        |[] -> [element]
        |head :: tail when element < head -> element :: lst
        |head :: tail -> head :: (insert_into_ordered_list (tail, element));;

let property_insert_keeps_ordered (lst:int list, element:int) =
    is_ordered_list lst ==> is_ordered_list (insert_into_ordered_list (lst, element))

printfn "*** property_insert_keeps_ordered";;
do Check.Quick property_insert_keeps_ordered;;

let property_first_is_smallest_if_ordered_list (lst:int list) =
    (lst <> []) ==> lazy ((List.sort lst |> List.head) = List.min lst)

printfn "*** property_first_is_smallest_if_ordered_list"
do Check.Quick property_first_is_smallest_if_ordered_list;;

let rec fetch_last lst =
    match lst with
        |[element] -> element
        |head :: tail -> tail |> fetch_last

let property_last_is_largest_if_ordered_list (lst:int list) =
    (lst <> []) ==> lazy ((List.sort lst |> fetch_last) = List.max lst)

printfn "*** property_last_is_largest_if_ordered_list"
do Check.Quick property_last_is_largest_if_ordered_list;;

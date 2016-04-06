#r @"FsCheck";;
open FsCheck;;

type category = Daycare | Nursery | Recreation;;
type name = string;;
type childDes = name * category;;

let rec number ((cat, lst):category * childDes list) =
    match lst with
        |[] -> 0
        |(n, c) :: xs when c = cat -> 1 + (number (cat, xs))
        |_ :: xs -> number (cat, xs);;

let p ((cat, lst):category * childDes list) = 
    number (cat, lst) <= List.length lst;;

do Check.Quick p;;

let pay (lst, name) =
    let rec remove lst =
        match lst with
            |[] -> ([], 0.)
            |(n, c) :: xs when n = name ->
                match c with
                    |Daycare -> (xs, 225.)
                    |Nursery -> (xs, 116.)
                    |Recreation -> (xs, 110.)
            |_ :: xs -> remove xs

    let rec fn lst res =
        match lst with
            |[] -> res 
            |(n, c) :: xs when n = name ->
                match c with
                    |Daycare -> fn xs res+112.5
                    |Nursery -> fn xs res+58.
                    |Recreation -> fn xs res+55.
            |_ :: xs -> fn xs res

    let (lst, n) = remove lst in fn lst n;;

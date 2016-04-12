#r @"FsCheck"
open FsCheck

(*
let rec dotProduct lst0 lst1 = 
    match (lst0, lst1) with
        |(_, []) -> 0
        |([], _) -> 0
        |(x::xs, y::ys) -> x * y + (dotProduct xs ys)

(*
let rec combine lst0 lst1 fn =
    match (lst0, lst1) with
        |(_, []) -> 0
        |([], _) -> 0
        |(x::xs, y::ys) -> (fn x y) + (combine xs ys fn)
*)

let _dotProduct lst0 lst1 =
    List.fold2 (fun acc e1 e2 -> acc + e1*e2) 0 lst0 lst1

let p0 (lst0:int list) (lst1:int list) (fn:int -> int -> int) =
    (List.length lst0 = List.length lst1) ==> lazy (dotProduct lst0 lst1 = _dotProduct lst0 lst1)

do Check.Quick p0

let rec takeWhile lst fn =
    match lst with
        |[] -> []
        |x::xs when not (fn x) -> [] 
        |x :: xs -> x :: (takeWhile xs fn)

let p1 (lst0:'a list) fn =
    let r = takeWhile lst0 fn
    List.length lst0 <= List.length r

let p2 (lst0:'a list) (fn:'a -> bool) =
    let r = takeWhile lst0 fn
    match (lst0, r) with
        |(_, []) -> true
        |([], _) -> true
        |(x::_, y::_) -> x = y

//do Check.Verbose p1

let rec dropWhile lst fn =
    match lst with
        |[] -> []
        |x::xs when fn x -> dropWhile xs fn
        |x::xs -> x::xs

let safeDiv (a:int option) (b:int option) =
    match (a, b) with
        |(None, _) -> None
        |(_, None) -> None
        |(Some x, Some y) when y <> 0 -> Some (x/y)
        |_ -> None

let optMapBinary a b fn =
    match (a, b) with
        |(None, _) -> None
        |(_, None) -> None
        |(Some x, Some y) -> Some (fn x y)

let optPlus a b = optMapBinary a b (+)

let optTimes a b = optMapBinary a b ( * )
*)

type form = 
    |F of bool
    |Not of form
    |And of form * form
    |Or of form * form

let rec eval f =
    match f with
        |F (e) -> e
        |Not (e) -> not (eval e)
        |And (e0, e1) -> (eval e0) && (eval e1)
        |Or (e0, e1) -> (eval e0) || (eval e1)

let rec toString f =
    match f with
        |F (e) -> sprintf "%b" e
        |Not (e) -> sprintf "- (%s)" (toString e)
        |And (e0, e1) -> sprintf "(%s /\ %s)" (toString e0) (toString e1)
        |Or (e0, e1) -> sprintf "(%s \/ %s)" (toString e0) (toString e1)

let main f =
    sprintf "Il risultato da valutare %s e' %b" (toString f) (eval f)

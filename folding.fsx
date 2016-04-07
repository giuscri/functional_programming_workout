#r "FsCheck"
open FsCheck

let concat lsts = List.foldBack (fun el acc -> el @ acc) lsts []

let filter fn lst =
    List.foldBack (fun el acc -> if fn el then el :: acc else acc) lst []

(*
let rec reduceBack (fn:'a -> 'a -> 'a) lst =
    match lst with
        |[] -> []
        |[x] -> [x]
        |x :: (y :: ys) -> reduceBack fn ((fn x y) :: ys)
*)

let rec pop_last lst =
    match lst with
        |[] -> failwith "..." 
        |[x] -> ([], x)
        |x :: xs -> let (p, q) = pop_last xs in (x :: p, q)

let reduceBack fn lst =
    let (p, q) = pop_last lst
    List.foldBack fn p q

let last lst = reduceBack (fun x y -> y) lst

let p (fn:int -> int -> int) (lst:int list) =
    lst <> [] ==> lazy (reduceBack fn lst = List.reduceBack fn lst)

do Check.Quick p

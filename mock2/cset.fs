module FSet

type FSet<'a when 'a : equality> = S of ('a -> bool)

let empty = S (fun _ -> false)

let contains e (S fn) = fn e

let add e (S fn) = S (fun x -> fn x || x = e)

let union (S fn0) (S fn1) = S (fun x -> fn0 x || fn1 x)

let rec ofList lst =
    match lst with
    | [] -> empty
    | x::xs -> add x (ofList xs)

let singleton e = ofList [e]

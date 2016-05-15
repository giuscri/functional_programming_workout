#r "FsCheck"
open FsCheck

let rec is_ordered lst =
    match lst with
        |[] -> true
        |[x] -> true
        |x::y::ys -> (x <= y) && (is_ordered (y::ys))

let rec insert_in_ordered lst el =
    match lst with
        |[] -> [el]
        |x::xs when x > el -> el::x::xs
        |x::xs -> x::(insert_in_ordered xs el)

let count_many lst value = List.foldBack (fun el acc -> if el = value then acc + 1 else acc) lst 0

let rec has_duplicates lst =
    match lst with
        |[] -> false
        |x::xs -> ((count_many xs x) > 0) || has_duplicates xs

let rec remove lst value =
    match lst with
        |[] -> []
        |x::xs when x = value -> remove xs value
        |x::xs -> x::(remove xs value)

let rec remove_duplicates lst =
    match lst with
        |[] -> []
        |x::xs -> x::(remove_duplicates (remove xs x))

let p0 arb (el:int) =
    Prop.forAll arb (fun lst ->
        is_ordered lst = is_ordered (insert_in_ordered lst el)
    )

let arb0 n m =
    Arb.from<int list>
    |> Arb.filter (fun lst -> n <= List.length lst)
    |> Arb.filter (fun lst -> List.length lst < m)
    |> Arb.mapFilter List.sort (fun lst -> not (has_duplicates lst)) 

let arb1 n m =
    Arb.from<int list>
    |> Arb.filter (fun lst -> n <= List.length lst)
    |> Arb.filter (fun lst -> List.length lst < m)
    |> Arb.mapFilter (fun lst -> remove_duplicates lst) (fun _ -> true)
    |> Arb.mapFilter List.sort (fun _ -> true) 

do Check.Quick (p0 (arb0 5 10))
do Check.Quick (p0 (arb1 5 10))

(* Roar! *)

let arb2 =
    Arb.from<int list * int list>
    |> Arb.filter (fun (l0, l1) -> List.length l0 = List.length l1)

let p1 arb =
    Prop.forAll arb (fun (l0, l1) ->
        let zipped = List.zip l0 l1
        let (r0, r1) = List.unzip zipped
        r0 = l0 && r1 = l1
    )

//do Check.Verbose (p1 arb2)

let smooth lst0 lst1 =
    let common_len = min (List.length lst0) (List.length lst1)
    (List.take (common_len) lst0, List.take (common_len) lst1)

let arb3 =
    Arb.from<int list * int list>
    |> Arb.mapFilter (fun (lst0, lst1) -> smooth lst0 lst1) (fun _ -> true)

// Way faster than `Check.Verbose (p1 arb2)`
do Check.Quick (p1 arb3)

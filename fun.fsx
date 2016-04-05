let rec map fn lst =
    match lst with
        |[] -> []
        |x :: xs -> (fn x) :: (map fn xs);;

let l1 = [1 .. 10]

let square = fun x -> (float x) ** 2. |> int;;
let l2 = l1 |> map square;;

//let l3 = l2 |> map (fun x -> (x, if x%2 = 0 then "pari" else "dispari"));;
let l3 = l2 |> map (fun x ->
    match x%2 with
        |0 -> (x, "pari")
        |_ -> (x, "dispari")
);;

let names = [
    ("Mario", "Rossi");
    ("Anna Maria", "Verdi");
    ("Giuseppe", "Di Gennaro");
];;

names |> map (fun p ->
    let (fname, lname) = p in
    sprintf "Dott. %s %s" fname lname;;
);;

let prop_map (f:int -> int) (lst:int list) =
    let r0 = lst |> map f in
    let r1 = lst |> List.map f in
    r0 = r1;;

#r @"FsCheck";;
open FsCheck;;
do Check.Quick prop_map;;

let rec filter fn lst =
    match lst with
        |[] -> []
        |x :: xs when fn x -> x :: filter fn xs
        |x :: xs -> filter fn xs;;

let multiple_of_three_up_to n =
    [1 .. n] |> filter (fun x -> x%3 = 0);;

let prop_filter (fn:int -> bool) (lst:int list) =
    let r0 = lst |> filter fn in
    let r1 = lst |> List.filter fn in
    r0 = r1;;

let rec len lst =
    match lst with
        |[] -> 0
        |x :: xs -> 1 + (len xs)

let prop_filter_len (fn:int -> bool) (lst:int list) =
    let filtered = lst |> filter fn in
    //List.length filtered <= List.length lst;;
    len filtered <= len lst;;

do Check.Quick prop_filter;;
do Check.Quick prop_filter_len;;

let rec _filter fn lst =
    match lst with
        |[] -> ([], [])
        |x :: xs when fn x ->
            let (oks, rejs) = _filter fn xs in
            (x :: oks, rejs)
        |x :: xs ->
            let (oks, rejs) = _filter fn xs in
            (oks, x :: rejs);;

let p = [1 .. 20] |> _filter (fun x -> x%3 = 0);;

let multiple_and_non_multiples_of_three_up_to n =
    [1 .. n] |> _filter (fun x -> x%3 = 0);;

let prop__filter_len (fn:int -> bool) (lst:int list) =
    let (oks, rejs) = _filter fn lst in
    len (oks @ rejs) = len lst;;

do Check.Quick prop__filter_len;;

let prop__filter_app (fn:int -> bool) (lst:int list) =
    let (oks, rejs) = _filter fn lst in
    List.sort (oks @ rejs) = List.sort lst;;

let divisors n =
    [1 .. n] |> filter (fun x -> x%n = 0);;

let is_prime n = divisors n |> len = 0;;

type valV = {
    nameV: string;
    dataV: int;
};;

type valG = {
    nameG: string;
    dataG: string;
};;

let valuta (r:valV) =
    let (n, d) = (r.nameV, r.dataV) in
    match d with
        |d when d < 18 ->
            {
                nameG = n;
                dataG = "insufficiente"
            }

        |d when d <= 22 ->
            {
                nameG = n;
                dataG = "sufficiente"
            }

        |d when d <= 27 -> 
            {
                nameG = n;
                dataG = "buono"
            }

        |d ->
            {
                nameG = n;
                dataG = "ottimo"
            };;

let rec valutaList (lst:valV list) =
    match lst with
        |[] -> []
        |x :: xs -> (valuta x) :: (valutaList xs);;

let ``valutaList e' una map di valuta`` (xs:valV list) =
    List.map valuta xs = valutaList xs;;

#r @"FsCheck";;
open FsCheck;;

do Check.Quick ``valutaList e' una map di valuta``;;

let rec creaValList ((lst0, lst1):string list * int list) =
    match (lst0, lst1) with
        |(_, []) -> []
        |([], _) -> []
        |(x :: xs, y :: ys) ->
            {nameV = x; dataV = y} :: (creaValList (xs, ys));;

let media (lst:valV list) =
    let rec sommaAndConta lst =
        match lst with
            |[] -> (0, 0)
            |x :: xs -> 
                let (n, res) = sommaAndConta xs in
                (1 + n, x.dataV + res) in

    match lst with
        |[] -> 0.
        |_ -> let (n, res) = sommaAndConta lst in (float res) / (float n);;

let rec separa (lst:valV list) =
    match lst with
        |[] -> ([], [])
        |x :: xs when x.dataV < 18 ->
            let (oks, rej) = separa xs in
                (oks, x :: rej)
        |x :: xs ->
            let (oks, rej) = separa xs in
                (x :: oks, rej);;

let ``due liste risultato hanno gli stessi elementi di vs`` (lst:valV list) =
    let (oks, rej) = separa lst in
    List.sort (oks @ rej) = List.sort lst;;

do Check.Quick ``due liste risultato hanno gli stessi elementi di vs``;;

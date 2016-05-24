// TO FINISH

type tp = INT | LSTINT

type exp =
    | K of int
    | Plus of exp * exp
    | Nil
    | Cons of exp * exp
    | Hd of exp
    | Tl of exp

type tpenvironment = Map<string,tp>

let rec tpcheck e =
    match e with
        | K (_) -> Some INT
        | Nil -> Some LSTINT
        | Plus (e0, e1) ->
            let (t0, t1) = (tpcheck e0), (tpcheck e1)
            match (t0, t1) with
                | (Some INT, Some INT) -> Some INT
                | (_, _) -> None

        | Cons (e0, e1) ->
            let (t0, t1) = (tpcheck e0), (tpcheck e1)
            match (t0, t1) with
                | (Some INT, Some LSTINT) -> Some LSTINT
                | (_, _) -> None

        | Hd (e) ->
            match (tpcheck e) with
                | Some LSTINT -> Some INT
                | _ -> None

        | Tl (e) ->
            match (tpcheck e) with
                | Some LSTINT -> Some LSTINT
                | _ -> None

#r "FsCheck"
open FsCheck

let test size len =
    let fn x y =
        match y with
            | Some (t) -> printf "%A has type %A\n" x t
            | None -> printf "%A is not typable\n" x

    let exps = Gen.sample size len Arb.generate<exp>
    List.map2 fn exps (List.map tpcheck exps)

let value e =
    match e with
        | K (_) | Nil -> true
        | Cons (e0, e1) ->
            match ((value e0), (value e1)) ->
                | (true, true) -> true
                | (_, _) -> false
        | _ -> false

let eval e =
    match e with
        | K v0 -> e

        | Plus (e0, e1) ->
            let (K v0) = eval e0
            let (K v1) = eval e1
            K (v0 + v1)

        | Nil -> Nil

        | Cons (e0, e1) ->
            let (K v0) = eval e0
            let (Tl v1) = eval e1
            Cons (v0, v1)

        | Hd e ->
            let (Cons (K v0, _)) = eval e
            K v0

        | Tl e ->
            let (Cons (_, ... v0)) = eval e

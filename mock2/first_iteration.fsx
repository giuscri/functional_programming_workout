#r "FsCheck"
open FsCheck

type boolex =
    | Const of bool
    | Var of char
    | Neg of boolex
    | And of boolex * boolex

type environment = char list

let rec eval exp env =
    match exp with
    | Const b -> b

    | Var c -> List.contains c env

    | Neg e -> not (eval e env)

    | And (e0, e1) -> (eval e0 env) && (eval e1 env)

let b1 = And (Var 'b', Var 'c')
let b2 = And (Neg b1, Const ((3 + 5) = 8))

let env1 = ['b'; 'c']
let env2 = ['d']

type ifexp =
    | K of bool
    | X of char
    | IF of ifexp * ifexp * ifexp

let rec ifeval exp env =
    match exp with
    | K b -> b

    | X c -> List.contains c env

    | IF (e0, e1, e2) ->
        match (ifeval e0 env, ifeval e1 env, ifeval e2 env) with
        | (true, b, _) -> b
        | (false, _, b) -> b

let rec bool2if exp =
    match exp with
    | Const b -> IF (K true, K b, K true)

    | Neg e -> IF (bool2if e, K false, K true)

    | And (e0, e1) ->
        IF (IF (bool2if e0, K true, K false), bool2if e1, K false)


let p0 =
    let rec fn exp = 
        match exp with
        | Const _ -> true
        | Var _ -> false
        | Neg e -> fn e
        | And (e0, e1) -> fn e0 && fn e1

    let arb = Arb.filter fn Arb.from<boolex>

    Prop.forAll arb (fun b ->
        ifeval (bool2if b) List.empty = eval b List.empty
    )

do Check.Verbose p0

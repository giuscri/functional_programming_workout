let nat = Seq.initInfinite (fun x -> x)

let seq1 =
    seq {
        yield 0
        yield 1
        yield 2
        yield 0
        yield 3
        yield 4
        yield 4
        yield 3
        yield 1
        yield! (Seq.initInfinite (fun x -> 5 + x))
    }

let rec _seq2 n =
    seq {
        yield n
        yield n
        yield! (_seq2 (n + 1))
    }

let seq2 = _seq2 0

let rec _seq3 n =
    seq {
        yield! (Seq.take n (Seq.initInfinite (fun x -> x)))
        yield! (_seq3 (n + 1))
    }

let seq3 = _seq3 2

let rec distinct s =
    seq {
        let first = Seq.item 0 s
        yield first
        yield! (distinct (Seq.filter (fun x -> x <> first) (Seq.skip 1 s)))
    }

let rec isEqual n s0 s1 =
    match n with
    | 0 -> true
    | n ->
        let (a, b) = ((Seq.item 0 s0), (Seq.item 0 s1))
        a = b && isEqual (n - 1) (Seq.skip 1 s0) (Seq.skip 1 s1)

let _ = isEqual 20 nat (distinct seq1)
let _ = isEqual 20 nat (distinct seq2)
let _ = isEqual 20 nat (distinct seq3)

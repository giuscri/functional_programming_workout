let nat = Seq.initInfinite (fun x -> x)

let rec map fn s =
    match (Seq.isEmpty s) with
        |true -> Seq.empty
        |false ->
            seq {
                yield fn (Seq.item 0 s)
                yield! (map fn (Seq.skip 1 s))
            }

let rec filter fn s =
    seq {
        if fn (Seq.item 0 s) then yield Seq.item 0 s
        yield! filter fn (Seq.skip 1 s)
    }

let rec fibFrom (a:uint64) (b:uint64) =
    seq {
        yield a
        yield! (fibFrom b (a + b))
    }

let sumSeq s =
    let rec fn acc s =
        seq {
            yield acc + (Seq.item 0 s)
            yield! fn (acc + (Seq.item 0 s)) (Seq.skip 1 s)
        }
    fn 0 s

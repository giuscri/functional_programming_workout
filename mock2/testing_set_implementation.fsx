#r "FsCheck"
#r "cset"

open FsCheck
open FSet

let p0 (e:int) (s0:Set<int>) =
    let s1 = ofList (Set.toList s0)
    Set.contains e s0 = contains e s1

do Check.Quick p0

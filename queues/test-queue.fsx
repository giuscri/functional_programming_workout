#r "QueueAsList"
open Queue

let q1 = put 10 (put 5 (put 3 empty))
let _, q2 = get q1
let _, q3 = get q2

let q4 = put 20 (put 15 q3)

let _, r0 = get q4
let _, r1 = get r0
let _, q5 = get r1

try
    let (head, tail) = get q5
    sprintf "%A" head
with
    | EmptyQueue -> "Whoops!"

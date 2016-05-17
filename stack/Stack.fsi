module Stack

type Stack<'a>
exception EmptyStack

val empty : 'a Stack

val push : 'a -> Stack<'a> -> Stack<'a>

val pop : Stack<'a> -> 'a * Stack<'a>

val top : Stack<'a> -> 'a

val size : Stack<'a> -> int

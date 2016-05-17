module Queue

type Queue<'a when 'a : equality>
exception EmptyQueue

val empty : Queue<'a>
val put : 'a -> Queue<'a> -> Queue<'a>
val get : Queue<'a> -> 'a * Queue<'a>

val isEmpty : Queue<'a> -> bool
val toList : Queue<'a> -> list<'a>
val ofList : list<'a> -> Queue<'a>
val put_list : list<'a> -> Queue<'a> -> Queue<'a>

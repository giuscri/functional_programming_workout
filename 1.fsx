// You can test it via `cat 1.fsx|fsharpi 2>/dev/null|egrep false`

let rec rm_even lst =
    match lst with
        |[] -> []
        |head :: tail when head%2=0 -> tail |> rm_even
        |head :: tail -> head :: (tail |> rm_even);;

rm_even [-10 .. 10] = [-9; -7; -5; -3; -1; 1; 3; 5; 7; 9];;

let rec rm_odd_position lst =
    match lst with
        |[] -> []
        |[x] -> [x]
        |[x0; x1] -> [x0]
        |x :: (y :: ys) -> x :: (ys |> rm_odd_position)

[0 .. 9] |> rm_odd_position = [0; 2; 4; 6; 8];;

let rec rm_even_position lst =
    match lst with
        |[] -> []
        |[x] -> [x]
        |[x0; x1] -> [x1]
        |x :: (y :: ys) -> y :: (ys |> rm_even_position);;

[0 .. 9] |> rm_even_position = [1; 3; 5; 7; 9];;

let split lst = lst |> rm_odd_position, lst |> rm_even_position;;

[0 .. 9] |> split = ([0; 2; 4; 6; 8], [1; 3; 5; 7; 9]);;

let rec cmp_length (lst0, lst1) =
    match (lst0, lst1) with
        |[], [] -> 0
        |[x], [] -> 1
        |[], [x] -> -1
        |x :: xs, y :: ys -> (xs, ys) |> cmp_length;;

([],[]) |> cmp_length = 0;;
([42],[]) |> cmp_length = 1;;
([],[42]) |> cmp_length = 1;;
([23; 110],[42; 89]) |> cmp_length = 0;;

let rec remove_element (to_remove, lst) =
    match lst with
        |[] -> []
        |x :: xs when x=to_remove -> (to_remove, xs) |> remove_element
        |x :: xs -> x :: ((to_remove, xs) |> remove_element);;

(4, [0 .. 10]) |> remove_element = [0 .. 3] @ [5 .. 10]

let rec remove_duplicates lst =
    match lst with
        |[] -> []
        |x :: xs -> x :: remove_duplicates (remove_element (x, xs));;

[0; 42; 42; 42; 42; 9] |> remove_duplicates = [0; 42; 9];;

let rec down_to_zero n =
    match n with
        |0 -> [0]
        |n -> n :: (n-1 |> down_to_zero);;

down_to_zero 10 = [10 .. -1 .. 0];;

let rec up_to n =
    match n with
        |0 -> [0]
        |n -> (n-1 |> up_to) @ [n];;

up_to 10 = [0 .. 10];;

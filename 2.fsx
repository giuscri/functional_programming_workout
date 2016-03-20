type figure =
    Rectangle of float * float
    | Square of float
    | Triangle of float * float;;

let compute_area f =
    match f with
        |Rectangle(b, h) when b >= 0. && h >= 0. -> Some (b * h)
        |Square(e) when e >= 0. -> Some (e * e)
        |Triangle(b, h) when b >= 0. && h >= 0. -> Some (b * h)
        |_ -> None;;

let print_area f =
    let area = f |> compute_area in
    match area with
        |Some x -> printfn "%f" x 
        |None -> printfn "Boss told me not to compute negative areas!";;

let sum_areas f0 f1 =
    let area0 = f0 |> compute_area in
    let area1 = f1 |> compute_area in
    match (area0, area1) with
        |(None, _) -> None
        |(_, None) -> None
        |(Some x, Some y) -> Some (x + y);;

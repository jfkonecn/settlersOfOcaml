let groupBy getKey l =
  let rec grouping acc = function
    | [] -> acc
    | hd :: tl ->
        let key = getKey hd in
        let l1, l2 = List.partition (fun x -> key = getKey x) tl in
        grouping ((key, hd :: l1) :: acc) l2
  in
  grouping [] l

let sortBy comparer getSortByProp l =
  let f x y = comparer (getSortByProp x) (getSortByProp y) in
  List.sort f l

let sortByChar getSortByProp l = sortBy Char.compare getSortByProp l

let sortByInt getSortByProp l = sortBy Int.compare getSortByProp l

let sortByFloat getSortByProp l = sortBy Float.compare getSortByProp l

let sortBy2TupleFloat getSortByProp l =
  let compare (x1, y1) (x2, y2) =
    let result = Float.compare x1 x2 in
    if result = 0 then Float.compare y1 y2 else result
  in
  sortBy compare getSortByProp l

let rec zip l1 l2 =
  match (l1, l2) with h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2 | _ -> []

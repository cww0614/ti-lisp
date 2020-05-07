let rec fold_map (f : 'a -> 'b -> 'a * 'c) (init : 'a) : 'b list -> 'a * 'c list
    = function
  | [] -> (init, [])
  | hd :: tl ->
      let acc, transformed = f init hd in
      let final, rest = fold_map f acc tl in
      (final, transformed :: rest)

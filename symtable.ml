module StringMap = Map.Make (String)

type 'a symbol_table = Table of 'a StringMap.t * 'a symbol_table | Top

let push table = Table (StringMap.empty, table)

let pop = function
  | Table (map, table) -> table
  | Top -> raise (Failure "Can not pop top table")

let add id value table =
  match table with
  | Table (map, parent) -> Table (StringMap.add id value map, parent)
  | Top -> raise (Failure "Adding to top symbol table")

let rec find id table =
  match table with
  | Table (map, parent) ->
      if StringMap.mem id map then Some (StringMap.find id map)
      else find id parent
  | Top -> None

let mem id table = match find id table with Some _ -> true | None -> false

let from decls =
  let initial = Table (StringMap.empty, Top) in
  List.fold_left
    (fun table decl ->
      let id, value = decl in
      add id value table)
    initial decls

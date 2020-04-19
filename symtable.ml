module StringMap = Map.Make (String)

type 'a symbol_table = Table of 'a StringMap.t * 'a symbol_table | Top

(* Create a new level of symbol table, and make the specified symbol
   table the upper level symbol table of this one *)
let push (table : 'a symbol_table) : 'a symbol_table =
  Table (StringMap.empty, table)

(* Discard the current level of symbol table and return the upper-level one *)
let pop : 'a symbol_table -> 'a symbol_table = function
  | Table (map, table) -> table
  | Top -> raise (Failure "Can not pop top table")

(* Add an entry to the symbol table *)
let add (id : string) (value : 'a) (table : 'a symbol_table) : 'a symbol_table =
  match table with
  | Table (map, parent) -> Table (StringMap.add id value map, parent)
  | Top -> raise (Failure "Adding to top symbol table")

(* Find the value associated with name recursively in the symbol table *)
let rec find (id : string) (table : 'a symbol_table) : 'a option =
  match table with
  | Table (map, parent) ->
      if StringMap.mem id map then Some (StringMap.find id map)
      else find id parent
  | Top -> None

(* Check if the name is in the symbol table recursively *)
let mem (id : string) (table : 'a symbol_table) : bool =
  match find id table with Some _ -> true | None -> false

(* Initialize a symbol table from given values. The values should be
   in the form [(name1, value1); (name2, value2); ...] *)
let from (decls : (string * 'a) list) : 'a symbol_table =
  let initial = Table (StringMap.empty, Top) in
  List.fold_left
    (fun table decl ->
      let id, value = decl in
      add id value table)
    initial decls

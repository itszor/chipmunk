(* Flatten AST lists: remove Vector instructions & NOPs *)

let flatten astlist =
  List.fold_right
    (fun node out ->
      match node with
        Ast.Null -> out
      | x -> x::out)
    astlist
    []

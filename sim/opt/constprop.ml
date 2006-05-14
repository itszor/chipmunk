(* Simple constant propagation from Appel:

  W <- a list of all statements in the SSA program
  while W is not empty
    remove some statement S from W
    if S is v <- phi(c, c, ..., c) for some constant c
      replace S by v <- c
    if S is v <- c for some constant c
      delete S from the program
      for each statement T that uses v
        substitute c for v in T
          W <- W U {T}

  Yet again, attack of the imperative algorithm!

  This really doesn't work very well with our (immutable) lists of AST nodes.
  
  We could do it with two passes, one which builds a collection of 
  deleted/rewritten nodes, the second which rewrites the AST. We should use
  an AstMap for this.
  
  Maybe we could share the edit list with later passes? Might be more 
  "efficient", as long as it doesn't get too large, but probably would
  rapidly become not worth it due to the extra complication involved.
  (Why not just have a mutable data structure if we're going to expend
  that much effort to fake one?)

  (Only works for int constants!)

*)

(* Turn a DynArray of vertices into a sequence of AST nodes *)
let stmtseq vertices =
  let last = (DynArray.length vertices)-1 in
  let rec walkv i stmtlist =
    match stmtlist with
      [] ->
        if i==last then
          Seq.Empty
        else
          let i = i+1 in
          let tag = DynArray.get vertices i in
          let Block.Block(astlist,term) = tag.Block.block in
          walkv i astlist
    | stmt::stmts -> Seq.cons stmt (walkv i stmts)
  in
    walkv (-1) []

(* Like List.for_all but over an array instead.
 * Empty array always gives `true'. (As does an empty list for List.for_all.)
 *)
let array_for_all fn ary =
  let length = Array.length ary in
  let rec scan n =
    if n=length then true
    else (fn ary.(n)) && scan (n+1)
  in
    scan 0

let all_constant carray =
  let length = Array.length carray in
  if length>0 then
    match carray.(0) with
      Ast.Constant(first) -> array_for_all
        (fun this ->
          match this with
            Ast.Constant(x) -> x=first
          | _ -> false)
        carray
    | _ -> false
  else
    false

let substitute_const const id ast =
  Ast.map_postorder
    (fun node ->
      match node with
        Ast.Register(id', Id.IntType, Ast.Use) when id=id' -> const
      | Ast.Register(id', Id.IntType, Ast.LastUse) when id=id' -> const
      | _ -> node)
    ast

let edit ~orig ~repl ~edits =
  if orig=repl then
    Printf.printf "Warning: orig=repl (%s)\n" (Disast.writeop orig);
  Ast.AstMap.add orig repl edits

let rec override ~edits ast =
  try
    let ast' = Ast.AstMap.find ast edits in
    override ~edits ast'
  with Not_found ->
    ast

let print_mods modmap =
  Printf.printf "Making modifications:\n";
  Ast.AstMap.iter
    (fun ast repl ->
      Printf.printf "ast:\n%s\n" (Disast.writeop ast);
      Printf.printf "replacement:\n%s\n" (Disast.writeop repl))
    modmap

let apply_mods vertices mods =
  for i=0 to (DynArray.length vertices)-1 do
    let tag = DynArray.get vertices i in
    let block = tag.Block.block in
    tag.Block.block <- Block.map_postorder
      (fun ast -> override ~edits:mods ast)
      block
  done

let propagate vertices usedefmap =
  let rec propagate' worklist edits =
    match worklist with
      Seq.Empty -> edits
    | Seq.Cons(s, rest) ->
        let worklist', edits' = Ast.fold_rtol_postorder
          (fun ast (worklist, edits) ->
            let ast = override ~edits ast in
            let ast =
              match ast with
                Ast.Move(dst, Ast.Phi(carray)) ->
                  if all_constant carray then
                    Ast.Move(dst, carray.(0))
                  else
                    ast
              | _ -> ast
            in
              match ast with
                Ast.Move(Ast.Register(id, Id.IntType, Ast.Assign) as dst,
                           ((Ast.Constant c) as const)) ->
                  let repl = Ast.Null in
                  let edits' = Ast.AstMap.add ast repl edits in
                  let rop = Ast.reg_or_pseudo_of_ast dst in
                  let usedef = Sets.RegOrPseudoMap.find rop usedefmap in
                  let stmts_use = usedef.Usedef.stmts_use in
                  List.fold_left
                    (fun (worklist, edits) (usestm,tag) ->
                      let repl = substitute_const const id usestm in
                      let edits' = edit ~orig:usestm ~repl ~edits in
                      Seq.cons repl worklist, edits')
                    (worklist, edits')
                    stmts_use
              | _ -> worklist, edits)
          s
          (Lazy.force rest, edits)
        in
          propagate' worklist' edits'
  in let seq = stmtseq vertices in
  let modifications = propagate' seq Ast.AstMap.empty in
  let empty = Ast.AstMap.is_empty modifications in
  print_mods modifications;
  apply_mods vertices modifications;
  not empty


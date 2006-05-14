(* Counts definitions and uses of regs/pseudo regs in a block
 * Now using SSA representation.
 *)

(* We will have a RegOrPseudoMap from a reg_or_pseudo to a usedef, where
 * a usedef looks like this:
 *)
type usedef = {
  mutable def : (Ast.ast * Block.tag) option;
  mutable stmts_use : (Ast.ast * Block.tag) list;
  mutable use : Ast.ast list
}

let add_definition rop ast block map =
  try
    let usedef = Sets.RegOrPseudoMap.find rop map in
    usedef.def <- Some (ast, block);
    map
  with Not_found ->
    Sets.RegOrPseudoMap.add rop
      {def=Some (ast, block); stmts_use=[]; use=[]} map

let add_use rop ast map =
  try
    let usedef = Sets.RegOrPseudoMap.find rop map in
    usedef.use <- ast :: usedef.use;
    map
  with Not_found ->
    Sets.RegOrPseudoMap.add rop {def=None; stmts_use=[]; use=[ast]} map

(* let add_def_or_use rop usage ast map =
  match usage with
    Ast.Use | Ast.LastUse -> add_use rop ast map
  | Ast.Assign -> add_definition rop ast map
  | Ast.FixedAssign -> add_use rop ast (add_definition rop ast map) *)

let add_use_and_def rop use def block map =
  add_use rop use (add_definition rop def block map)

(* For definitions, record the enclosing Move ast.
 * For uses, point to the reg/pseudo ast itself.
 *
 * Arg, uses also need to know the enclosing stmt for simple constant 
 * propagation. So we will make each use a pair of (actual use, enclosing stmt).
 *)
let count_uses_and_defs tag map =
  let block = tag.Block.block in
  Block.block_fold_backwards
    (fun ast map ->
        match ast with
          Ast.Move(Ast.Register(id, typ, Ast.Assign), _) ->
            add_definition (Id.PhysReg(id, typ)) ast tag map
        | Ast.Move(Ast.Register(id, typ, Ast.FixedAssign) as use, _) ->
            add_use_and_def (Id.PhysReg(id, typ)) use ast tag map
        | Ast.Move(Ast.Pseudo(ps, typ, Ast.Assign), _) ->
            add_definition (Id.PseudoReg(ps,typ)) ast tag map
        | Ast.Move(Ast.Pseudo(ps, typ, Ast.FixedAssign) as use, _) ->
            add_use_and_def (Id.PseudoReg(ps,typ)) use ast tag map
        | Ast.Register(id, typ, Ast.Use)
        | Ast.Register(id, typ, Ast.LastUse) ->
            add_use (Id.PhysReg(id,typ)) ast map
        | Ast.Pseudo(ps, typ, Ast.Use)
        | Ast.Pseudo(ps, typ, Ast.LastUse) ->
            add_use (Id.PseudoReg(ps,typ)) ast map
        | _ -> map)
    block
    map

(* Find a list of ast (register|pseudos) which are used by a higher-level ast *)
let ast_uses ast =
  Ast.fold_ltor_postorder
    (fun ast uses ->
      match ast with
        Ast.Register(_, _, Ast.Use)
      | Ast.Register(_, _, Ast.LastUse)
      | Ast.Pseudo(_, _, Ast.Use)
      | Ast.Pseudo(_, _, Ast.LastUse) -> ast::uses
      | _ -> uses)
    ast
    []

(* Fill in stmts_use field in usedefmap: that is, for a particular definition,
 * a list of statements that the definition is used by.
 *)
let count_stmts_use usedefmap =
  Sets.RegOrPseudoMap.iter
    (fun rop usedef ->
      match usedef.def with
        None -> ()
      | Some (def, containing_tag) ->
          let uses = ast_uses def in
          List.iter
            (fun use ->
              let rop = Ast.reg_or_pseudo_of_ast use in
              let thisusedef = Sets.RegOrPseudoMap.find rop usedefmap in
              thisusedef.stmts_use <-
                (def, containing_tag) :: thisusedef.stmts_use)
            uses)
    usedefmap    

let count vertices =
  DynArray.fold_left
    (fun map tag -> count_uses_and_defs tag map)
    Sets.RegOrPseudoMap.empty
    vertices

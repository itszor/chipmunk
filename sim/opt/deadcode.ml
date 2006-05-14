(* Dead code elimination *)

(* If a statement defines a variable and it is never used, the statement is 
 * dead.
 *
 * What about definitions which fall outside our known code? They must never
 * be deleted! Now, these are written to with "FixedAssign" so we know to not
 * delete them.
 *
 * Current implementation isn't very efficient -- check use of List.filter!
 *)

open Usedef

(* Physical registers with suffix zero are always defined before our
 * local fragment, so don't count them as being used but not defined.
 *)
let suppress_initial = function
    Id.PhysReg((_, Id.Suf(0)), _) -> true
  | _ -> false

(* To start with, just see if there are any definitions with no uses. *)
let check_defs usedefmap =
  Sets.RegOrPseudoMap.iter
    (fun rop usedef ->
      match usedef.def, usedef.use with
        Some _, [] ->
          Printf.printf "Def with no use! (%s)\n"
            (Id.string_of_rop rop)
      | None, [] ->
          Printf.printf "No use or def? (%s)\n"
            (Id.string_of_rop rop)
      | None, l ->
          let len = List.length l in
          let suff = if len==1 then "" else "s" in
          if not (suppress_initial rop) then
            Printf.printf "%d use%s with no def (%s)\n"
              len suff (Id.string_of_rop rop)
      | _ -> ())
    usedefmap

(*

This is the SSA dead-code elimination algorithm given by Appel.

  W <- a list of all variables in the SSA program
  while W is not empty
    remove some variable v from W
    if v's list of uses is empty
      let S by v's statement of definition
      if S has no side effects other than the assignment to v
        delete S from the program
        for each variable x[i] used by S
          delete S from the list of uses of x[i]
          W <- W U {x[i]}

This might need a little rearrangement before it works nicely with the data
structures we have.

*)

let rec remove_uses ~map:usedefmap ~def ~uses ~worklist =
  match uses with
    [] -> worklist
  | xi::xis ->
    let xiusedef =
      Sets.RegOrPseudoMap.find (Ast.reg_or_pseudo_of_ast xi) usedefmap in
    xiusedef.use <- List.filter (fun use -> use <> def) xiusedef.use;
    remove_uses ~map:usedefmap ~def:def ~uses:xis ~worklist:(xi::worklist)

let assigned_reg def =
  match def with
    Ast.Move(dst, _) -> dst
  | _ -> failwith "Unrecognized assignment"

let all_defs usedefmap =
  Sets.RegOrPseudoMap.fold
    (fun rop usedef defs ->
      match usedef.def, usedef.use with
        Some (x, _), _ -> (assigned_reg x)::defs
      | _ -> defs)
    usedefmap
    []

let delete_dead_statements vertices deadset =
  for i=0 to (DynArray.length vertices)-1 do
    let rec walk astlist acc =
      match astlist with
        [] -> List.rev acc
      | stmt::stmts ->
        if Ast.AstSet.mem stmt deadset then
          walk stmts acc
        else
          walk stmts (stmt::acc)
    in
      let tag = DynArray.get vertices i in
      let Block.Block(astlist, term) = tag.Block.block in
      tag.Block.block <- Block.Block(walk astlist [], term)
  done

(* Now def points at the enclosing Move ast.
 * build up a set of asts to remove, then delete them all in a post-pass.
 *)
let remove vertices usedefmap =
  let rec remove' worklist deleteme =
    match worklist with
      [] -> deleteme
    | v::ws ->
        let usedef = Sets.RegOrPseudoMap.find
                       (Ast.reg_or_pseudo_of_ast v) usedefmap in
        begin match usedef.def, usedef.use with
          Some (def,tag), [] ->
            (* A definition with no uses *)
            begin match Stmt.classify def with
              Stmt.Assignment
            | Stmt.PhiFunc ->
                let uses = Usedef.ast_uses def in
                let worklist' =
                  remove_uses ~map:usedefmap
                              ~def:(assigned_reg def)
                              ~uses:uses
                              ~worklist:ws in
                remove' worklist' (Ast.AstSet.add def deleteme)
            | _ -> remove' ws deleteme
            end
        | _ -> remove' ws deleteme
        end
  in
    let deleteme = remove' (all_defs usedefmap) Ast.AstSet.empty in
    delete_dead_statements vertices deleteme
(*    Printf.printf "Deleting insns:\n";
    Disast.writeinsns deleteme;
    Printf.printf "End deleted insn list\n"*)


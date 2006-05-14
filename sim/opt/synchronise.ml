(* Synchronise the state of registers at block exit points.

   At this point, we are dealing with hardware registers only: no subscripts,
   and no pseudo registers.
   
   At each synchronisation point, we should scan backwards through the block
   graph (by immediate dominators, which should have been calculated
   previously) looking for expiry of each hard register in the 
   compiler-generated set (ie, not the optimiser registers r31-r53, r55 
   (special), f36-f63).

   (Because it's more efficient, we actually scan and rewrite the blocks
   in a single forward pass over the immediate-dominator tree (i.e. using the
   immediate-dominator children). We use an immutable set to make this rather
   easy.

   Syncreg nodes are rewritten as vectors of FixedAssign moves, containing the 
   registers which don't expire.
*)

module RoP = Sets.RegOrPseudoSet

let from_to = Common.from_to

(* A RegOrPseudoSet of the registers which should be preserved for
   statically-compiled code.
*)
let static_reg_set () =
  let set = RoP.empty in
  let set = from_to
    (fun n a -> RoP.add (Id.PhysReg((n, Id.Unset), Id.IntType)) a) set 0 30 in
  let set = from_to
    (fun n a -> RoP.add (Id.PhysReg((n, Id.Unset), Id.IntType)) a) set 54 54 in
  let set = from_to
    (fun n a -> RoP.add (Id.PhysReg((n, Id.Unset), Id.IntType)) a) set 56 63 in
  let set = from_to
    (fun n a -> RoP.add (Id.PhysReg((n, Id.Unset), Id.FloatType)) a) set 0 35 in
  set

let expire_from ast alive =
  match ast with
    Ast.Register(id, typ, Ast.LastUse) ->
      RoP.remove (Id.PhysReg(id, typ)) alive
  | Ast.Register(id, typ, Ast.Assign)
  | Ast.Register(id, typ, Ast.FixedAssign) ->
      RoP.add (Id.PhysReg(id, typ)) alive
  | _ -> alive

(* Preserve (FixedAssign-write) all registers in the alive set *)
let preserve alive tail =
  RoP.fold
    (fun rop olist ->
      let insn =
        match rop with
          Id.PhysReg(_, Id.FloatType) ->
            Ast.Move(Ast.ast_of_reg_or_pseudo rop Ast.FixedAssign,
                     Ast.Unop(Ast.Cast_f Iformat.Double,
                       Ast.ast_of_reg_or_pseudo rop Ast.Use))
        | Id.PhysReg _ ->
            Ast.Move(Ast.ast_of_reg_or_pseudo rop Ast.FixedAssign,
                     Ast.ast_of_reg_or_pseudo rop Ast.Use)
        | _ -> failwith "Unexpected reg_or_pseudo type"
      in insn :: olist)
    alive
    tail

let map_astlist astlist alive =
  let rec scan astlist alive rewritten =
    match astlist with
      [] -> alive, rewritten
    | Ast.Syncregs::rest ->
        scan rest alive (preserve alive rewritten)
    | ast::rest ->
        let alive' = Ast.fold_rtol_postorder expire_from ast alive in
        scan rest alive' (ast :: rewritten)
  in
    let alive', astlist' = scan astlist alive [] in
    alive', List.rev astlist'

let sync vertices =
  let rec rewrite tag alive =
    let Block.Block(astlist, term) = tag.Block.block in
    let alive', astlist' = map_astlist astlist alive in
    let alive'' =
      Block.apply_term_as_ast_backwards expire_from term alive' in
    tag.Block.block <- Block.Block(astlist', term);
    List.iter (fun b -> rewrite b alive'') tag.Block.idomchild;
  in
    rewrite (DynArray.get vertices 0) (static_reg_set ())

(*  
  for i=last downto 0 do
    let tag = DynArray.get vertices i in
    let rec rewrite_sync = function
      [] -> []
    | Ast.Syncregs :: above -> (non_expired_set above tag) :: rewrite_sync above
    | x :: above -> x :: rewrite_sync above
    in let Block.Block(astlist,term) = tag.Block.block in
    let astlist' = rewrite_sync astlist in
    tag.Block.block <- Block(astlist', term)
  done
*)



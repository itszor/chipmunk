(* Register precolouring/preallocation

  here, we request allocation in fixed registers for the initial regs

     r0[0]..r63[0] and f0[0]..f63[0]

  and any regs which are written with a FixedAssign move to their 
  non-subscripted hardware registers.

  If these sets ever interfere, it is caught during colouring.

  Register precolourings are stored in a RegOrPseudoMap (target regs
  should have no suffix).
*)

let unsubscripted = function
    Id.PhysReg((n,_), idtype) -> Id.PhysReg((n, Id.Unset), idtype)
  | _ -> failwith "Can't fixed-assign a pseudo register!"

(* Look through set of blocks; find the FixedAssigns to hard regs and the hard
   registers with suffix 0 (the "input" regs for the block).  *)
let find_fixed_assignments vertices =
  DynArray.fold_right
    (fun tag regset ->
      Block.block_fold_backwards
        (fun ast regset' ->
          match ast with
            Ast.Register(_, _, Ast.FixedAssign)
          | Ast.Pseudo(_, _, Ast.FixedAssign)
          | Ast.Register((_, Id.Suf 0), _, (Ast.Use | Ast.LastUse)) ->
              let rop = Ast.reg_or_pseudo_of_ast ast in
              let unsub = unsubscripted rop in
              Sets.RegOrPseudoMap.add rop unsub regset'
          | _ -> regset'
        )
        tag.Block.block
        regset)
    vertices
    Sets.RegOrPseudoMap.empty

let write_fixed_assignments fixed =
  Sets.RegOrPseudoMap.iter
    (fun rop unsub -> Printf.printf "%s -> %s\n" (Id.string_of_rop rop)
      (Id.string_of_rop unsub))
    fixed


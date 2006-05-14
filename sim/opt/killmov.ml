(* Kill redundant move instructions (register -> same register).
   Preservation of register-expiry bits doesn't need to be handled explicitly:
   the same register always comes alive here by definition.  *)

let movfilter insn ilist =
  match insn with
    Iformat.Alu2F(Iformat.Mov, Iformat.Dreg(rd), Iformat.Reg(rs, _))
      when rd=rs -> ilist
  | Iformat.Alu2fF(Iformat.Movf, Iformat.Double,
                     Iformat.Dfreg(rd), Iformat.Freg(rs, _))
      when rd=rs -> ilist
  | _ -> insn :: ilist

let killmov vertices =
  let num = DynArray.length vertices in
  for i=0 to num-1 do
    let tag = DynArray.get vertices i in
    let selected = List.fold_right
      (fun sel acc ->
         let new_sasm = match sel.Select.s_asm with
           Ast.Ainsn(ilist) ->
             Ast.Ainsn(List.fold_right movfilter ilist [])
         | _ -> failwith "Non-allocated insn or phi node"
         in
           match new_sasm with
             Ast.Ainsn [] -> acc
           | _ -> { sel with Select.s_asm = new_sasm } :: acc)
      tag.Block.selected
      []
    in
      tag.Block.selected <- selected
  done

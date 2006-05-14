(* Rewrite virtual registers (subscripted and pseudo) into hardware registers,
   using an allocation map.  *)

open Block

let insnlist state il =
  String.concat "; " (List.map (Disassemble.diss state) il)

(* Try inserting dummy register allocations so that we can print a failing
   instruction.  *)
let diagnose state insn alloc fa =
  let rec retry alloc fa =
    let id = Ast.reg_or_pseudo_of_ast fa in
    let alloc' =
      Sets.RegOrPseudoMap.add id
      (Id.PhysReg ((9999, Id.Unset), Id.IntType)) alloc
    in
      begin try
        let solid = insn alloc' in
        failwith ("Reg " ^ (Id.string_of_rop id) ^ " in insn: " ^
          (insnlist state solid))
      with Asmsupport.FailedAlloc fa ->
        retry alloc' fa
      end
  in
    retry alloc fa

let rewrite state vertices alloc =
  let num = DynArray.length vertices in
  for i=0 to num-1 do
    let tag = DynArray.get vertices i in
    let selected' = List.map
      (fun si ->
        match si.Select.s_asm with
          Ast.Insn i ->
            begin try
              let ai = i alloc in
              (* Printf.printf "Allocated: %s\n" (insnlist state ai); *)
              {si with Select.s_asm = Ast.Ainsn ai}
            with Asmsupport.FailedAlloc fa ->
              diagnose state i alloc fa
            end
        | Ast.Iphi _ -> failwith "Can't have phi node here"
        | _ -> si)
      tag.selected
    in
      tag.selected <- selected'
  done

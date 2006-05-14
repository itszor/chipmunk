open Ast
open Id
open I32op

exception Done of Block.terminate

let transLife = function
  Iformat.Alive -> Use
| Iformat.Dead -> LastUse

let transReg = function
  Iformat.Reg(num,life) -> Register((num, Unset), IntType, transLife life)

let transFreg = function
  Iformat.Freg(num,life) -> Register((num, Unset), FloatType, transLife life)

let setReg (Iformat.Dreg num) =
  Register((num, Unset), IntType, Assign)

let setFreg (Iformat.Dfreg num) =
  Register((num, Unset), FloatType, Assign)

let srcifyReg (Iformat.Dreg num) usage =
  Register((num, Unset), IntType, usage)

let transFimm x = Floatconst (Int32.to_float x)

let transWidth = function
    Iformat.Byte -> Byte
  | Iformat.Halfword -> Halfword
  | Iformat.Word -> Word

let transPrec = function
    Iformat.Single -> Word
  | Iformat.Double -> Doubleword

(* let extractDom d =
  let Iformat.Domain(d') = d in d' *)

let transInd vol width =
  match vol with
    Iformat.Volatile -> IndVol width
  | Iformat.Nonvolatile -> Ind width

let transTraploc = function
    Iformat.Bits(n) -> Constant(!! n)
  | Iformat.Srcreg(r) -> transReg r

let transAlu2 opc dest src =
  match opc with
    Iformat.Mov -> Move(setReg dest, transReg src)
  | Iformat.Not -> Move(setReg dest, Unop(Not, transReg src))

let transAlui2 opc dest imm =
  match opc with
    Iformat.Mov -> Move(setReg dest, (Constant imm))
  | Iformat.Not -> Move(setReg dest, (Constant (Int32.lognot imm)))

let transAlu3 opc dest src1 src2 =
  let binop code = Move(setReg dest,
                     Binop(code, transReg src1, transReg src2))
  in match opc with
    Iformat.Lsl -> binop Lsl
  | Iformat.Lsr -> binop Lsr
  | Iformat.Asr -> binop Asr
  | Iformat.Ror -> binop Ror
  | Iformat.And -> binop And
  | Iformat.Ior -> binop Ior
  | Iformat.Eor -> binop Eor
  | Iformat.Bic -> Move(setReg dest,
                     Binop(And, transReg src1, Unop(Not, transReg src2)))
  | Iformat.Add -> binop Add
  | Iformat.Sub -> binop Sub
  | Iformat.Rsb -> Move(setReg dest,
                     Binop(Sub, transReg src2, transReg src1))
  | Iformat.Mul -> binop Mul
  | Iformat.Div -> binop Div
  | Iformat.Udiv -> binop Udiv
  | Iformat.Mod -> binop Mod
  | Iformat.Umod -> binop Umod
  | Iformat.Mlh -> binop Mlh
  | Iformat.Umlh -> binop Umlh

let transAlui3 opc dest src1 imm =
  let binop code = Move(setReg dest,
                     Binop(code, transReg src1, Constant imm))
  in match opc with
    Iformat.Lsl -> binop Lsl
  | Iformat.Lsr -> binop Lsr
  | Iformat.Asr -> binop Asr
  | Iformat.Ror -> binop Ror
  | Iformat.And -> binop And
  | Iformat.Ior -> binop Ior
  | Iformat.Eor -> binop Eor
  | Iformat.Bic -> Move(setReg dest,
                     Binop(And, transReg src1, Constant (Int32.lognot imm)))
  | Iformat.Add -> binop Add
  | Iformat.Sub -> binop Sub
  | Iformat.Rsb -> Move(setReg dest,
                     Binop(Sub, Constant imm, transReg src1))
  | Iformat.Mul -> binop Mul
  | Iformat.Div -> binop Div
  | Iformat.Udiv -> binop Udiv
  | Iformat.Mod -> binop Mod
  | Iformat.Umod -> binop Umod
  | Iformat.Mlh -> binop Mlh
  | Iformat.Umlh -> binop Umlh

let transAlu2f opc prec rd rn =
  let new_opcode =
    match opc with
      Iformat.Movf -> Cast_f prec
    | Iformat.Negf -> Neg_f prec
    | Iformat.Absf -> Abs_f prec
    | Iformat.Sqrf -> Sqr_f prec
  in
    Move(setFreg rd, Unop(new_opcode, transFreg rn))

let transAlu2fi opc prec rd imm =
  let new_opcode =
    match opc with
      Iformat.Movf -> Cast_f prec
    | Iformat.Negf -> Neg_f prec
    | Iformat.Absf -> Abs_f prec
    | Iformat.Sqrf -> Sqr_f prec
  in
    Move(setFreg rd, Unop(new_opcode, transFimm imm))

let transAlu3f opc prec rd rm rn =
  let new_opcode =
    match opc with
      Iformat.Addf -> Add_f prec
    | Iformat.Subf -> Sub_f prec
    | Iformat.Mulf -> Mul_f prec
    | Iformat.Divf -> Div_f prec
  in
    Move(setFreg rd, Binop(new_opcode, transFreg rm, transFreg rn))

let transAlu3fi opc prec rd rm imm =
  let new_opcode =
    match opc with
      Iformat.Addf -> Add_f prec
    | Iformat.Subf -> Sub_f prec
    | Iformat.Mulf -> Mul_f prec
    | Iformat.Divf -> Div_f prec
  in
    Move(setFreg rd, Binop(new_opcode, transFreg rm, transFimm imm))

let transFix prec rd rs =
  Move(setReg rd, Unop(Cast_f prec, transFreg rs))

let transFlt prec rd rs =
  Move(Unop(Cast_f prec, setFreg rd), transReg rs)

let transLdf rd rb ri vol prec =
  Move(Unop(Cast_f prec, setFreg rd),
    Unop(transInd vol (transPrec prec),
         Binop(Add, transReg rb, transReg ri)))

let transLdfi rd rb offset vol prec =
  Move(Unop(Cast_f prec, setFreg rd),
    Unop(transInd vol (transPrec prec),
         Binop(Add, transReg rb, Constant offset)))

let transStf rd rb ri vol prec =
  Move(Unop(transInd vol (transPrec prec),
            Binop(Add, transReg rb, transReg ri)),
       Unop(Cast_f prec, transFreg rd))

let transStfi rd rb offset vol prec =
  Move(Unop(transInd vol (transPrec prec),
            Binop(Add, transReg rb, Constant offset)),
       Unop(Cast_f prec, transFreg rd))

let transMvc dest imm fill replace part =
  match replace with
    Iformat.Replace ->
      let shiftedimm =
        match part with
          Iformat.Lo -> imm
        | Iformat.Hi -> (Int32.shift_left imm 16)
      in let otherbits =
        match fill with
          Iformat.Zeros -> Int32.zero
        | Iformat.Ones -> Int32.of_int 0xffff
      in let shiftedotherbits =
        match part with
          Iformat.Lo -> (Int32.shift_left otherbits 16)
        | Iformat.Hi -> otherbits
      in let finalval =
        Int32.logor shiftedimm shiftedotherbits
      in
        Move(setReg dest, Constant finalval)
  | Iformat.Merge ->
    let andval =
      Int32.shift_left (Int32.of_int 0xffff)
        (match part with Iformat.Lo -> 16 | Iformat.Hi -> 0)
    and orval =
      Int32.shift_left imm
        (match part with Iformat.Lo -> 0 | Iformat.Hi -> 16)
    in
      Move(setReg dest, Binop(Ior, Binop(And, srcifyReg dest LastUse,
        Constant andval), Constant orval))

let transCmp cond dest cmp1 cmp2 =
  Move(setReg dest, Binop(Cmp(cond), transReg cmp1, transReg cmp2))

let transCmpi cond dest cmp1 imm =
  Move(setReg dest, Binop(Cmp(cond), transReg cmp1, Constant imm))

let transStr src base index vol width =
  Move(Unop(transInd vol (transWidth width),
    Binop(Add, transReg base, transReg index)), transReg src)

let transStri src base offset vol width =
  Move(Unop(transInd vol (transWidth width),
    Binop(Add, transReg base, Constant offset)), transReg src)

let transLdr dest base index vol width =
  Move(setReg dest, Unop(transInd vol (transWidth width),
    Binop(Add, transReg base, transReg index)))

let transLdri dest base offset vol width =
  Move(setReg dest, Unop(transInd vol (transWidth width),
    Binop(Add, transReg base, Constant offset)))

let transBfx dest from lo hi signx =
  let lo',hi' = Int32.of_int lo,Int32.of_int hi in
  match signx with
    Iformat.Zeroext ->
      Move(setReg dest, Triop(Bitfield, Constant lo', Constant hi',
                              transReg from))
  | Iformat.Signext ->
      Move(setReg dest, Triop(SignedBitfield, Constant lo', Constant hi',
                              transReg from))

let transSpl dest lhs rhs shft pos =
  match shft with
    Iformat.SpliceShift ->
      Move(setReg dest, Triop(SpliceShift,
        transReg lhs, transReg rhs,
        Constant (Int32.of_int pos)))
  | Iformat.Splice ->
      Move(setReg dest, Triop(Splice,
        transReg lhs, transReg rhs,
        Constant (Int32.of_int pos)))

let reglist usetype regtype b a =
  let li = ref [] in
  begin if b>a then
    for i = a to b do
      li := Register((i, Unset), regtype, usetype) :: !li
    done
  else
    for i = a downto b do
      li := Register((i, Unset), regtype, usetype) :: !li
    done
  end;
  !li

(* and transDir = function
  Iformat.Inc -> Inc
| Iformat.Dec -> Dec *)

(*  and transWhen = function
  Iformat.Before -> Before
| Iformat.After -> After *)

let mmem ~basereg:rb ~regs ~writeback:wb ~insngen ~start ~offset ~acc =
  let base = srcifyReg rb Use
  and dbase = setReg rb in
  let nregs = Array.length regs in
  let rec make offs update i =
    if i=nregs then
      acc
    else
      let insn = insngen ~reg:regs.(i) ~base ~offset:offs in
      insn :: make (Int32.add offs update) update (i+1)
  in
    let insns = make start offset 0 in
    match wb with
      Iformat.Writeback ->
        let update =
          Move(dbase,
            Binop(Add, base,
                       Constant ((Int32.mul (Int32.of_int nregs)
                                            offset))))
        in
          update :: insns
    | Iformat.Discard -> insns

let transLdm rb dra up wb acc =
  let indir = Ind Word in
  let loadgen ~reg ~base ~offset =
    Move(setReg reg, Unop(indir, Binop(Add, base, Constant offset)))
  in let start, offset =
    match up with
      Iformat.IncAfter -> 0l, 4l
    | Iformat.DecBefore -> -4l, -4l
  in
    mmem ~basereg:rb ~regs:dra ~writeback:wb ~insngen:loadgen ~start ~offset
         ~acc

let transStm rb sra up wb acc =
  let indir = Ind Word in
  let storegen ~reg ~base ~offset =
    Move(Unop(indir, Binop(Add, base, Constant offset)), transReg reg)
  in let start, offset =
    match up with
      Iformat.IncAfter -> 0l, 4l
    | Iformat.DecBefore -> -4l, -4l
  in
    mmem ~basereg:rb ~regs:sra ~writeback:wb ~insngen:storegen ~start ~offset
         ~acc

let transLdmf rb dra up wb acc =
  let indir = Ind Doubleword in
  let loadgen ~reg ~base ~offset =
    Move(setFreg reg, Unop(indir, Binop(Add, base, Constant offset)))
  in let start, offset =
    match up with
      Iformat.IncAfter -> 0l, 8l
    | Iformat.DecBefore -> -8l, -8l
  in
    mmem ~basereg:rb ~regs:dra ~writeback:wb ~insngen:loadgen ~start ~offset
         ~acc

let transStmf rb sra up wb acc =
  let indir = Ind Doubleword in
  let storegen ~reg ~base ~offset =
    Move(Unop(indir, Binop(Add, base, Constant offset)), transFreg reg)
  in let start, offset =
    match up with
      Iformat.IncAfter -> 0l, 8l
    | Iformat.DecBefore -> -8l, -8l
  in
    mmem ~basereg:rb ~regs:sra ~writeback:wb ~insngen:storegen ~start ~offset
         ~acc

let transLdt opc imm =
  match opc with
    Iformat.Ldx -> Move(Treg X, Constant imm)
  | Iformat.Ldy -> Move(Treg Y, Constant imm)
  | Iformat.Ldz -> Move(Treg Z, Constant imm)

let transTrap tcond rc loc =
  Binop(Trap(tcond), transReg rc, transTraploc loc)

let transSwi code =
  Unop(Swi, Constant code)

let transDest = function
    Iformat.Bits(n) -> Block.PartialDirect(!! n)
  | Iformat.Srcreg(Iformat.Reg(num,expir)) ->
      Block.Indirect(Register((num, Unset), IntType, transLife expir))

let translist ilist =
  let rec scan ilist acc =
    match ilist with
      [] -> failwith "List without terminator"
    | Iformat.CbrF(rc, tru, fal) :: _ ->
        let Iformat.Reg(num, expir) = rc in
        let term = Block.CondBranch(0.5, Register((num, Unset), IntType,
                                    transLife expir),
                              transDest tru, transDest fal) in
        Block.Block(acc, term)
    | Iformat.CallF(cal, ret) :: _ ->
        let term = Block.Call(transDest cal, transDest ret) in
        Block.Block(acc, term)
    | Iformat.JumpF(dest) :: _ ->
        let term = Block.Jump(transDest dest) in
        Block.Block(acc, term)
    | Iformat.RetF :: _ ->
        Block.Block(acc, Block.Return)
    | Iformat.Alu2F(opc,rd,rn) :: rest ->
        scan rest (transAlu2 opc rd rn :: acc)
    | Iformat.Alu2iF(opc,rd,imm) :: rest ->
        scan rest (transAlui2 opc rd imm :: acc)
    | Iformat.Alu3F(opc,rd,rm,rn) :: rest ->
        scan rest (transAlu3 opc rd rm rn :: acc)
    | Iformat.Alu3iF(opc,rd,rm,imm) :: rest ->
        scan rest (transAlui3 opc rd rm imm :: acc)
    | Iformat.MvcF(rd,imm,fill,replace,part) :: rest ->
        scan rest (transMvc rd imm fill replace part :: acc)
    | Iformat.CmpF(cond,rd,rm,rn) :: rest ->
        scan rest (transCmp cond rd rm rn :: acc)
    | Iformat.CmpiF(cond,rd,rm,imm) :: rest ->
        scan rest (transCmpi cond rd rm imm :: acc)
    | Iformat.StrF(rd,rb,ri,vol,width) :: rest ->
        scan rest (transStr rd rb ri vol width :: acc)
    | Iformat.StriF(rd,rb,offset,vol,width) :: rest ->
        scan rest (transStri rd rb offset vol width :: acc)
    | Iformat.LdrF(rd,rb,ri,vol,width) :: rest ->
        scan rest (transLdr rd rb ri vol width :: acc)
    | Iformat.LdriF(rd,rb,offset,vol,width) :: rest ->
        scan rest (transLdri rd rb offset vol width :: acc)
    | Iformat.LdmF(rb,dra,up,wb) :: rest ->
        scan rest (transLdm rb dra up wb acc)
    | Iformat.StmF(rb,sra,up,wb) :: rest ->
        scan rest (transStm rb sra up wb acc)
    | Iformat.BfxF(rd,rm,lo,hi,signx) :: rest ->
        scan rest (transBfx rd rm lo hi signx :: acc)
    | Iformat.SplF(rd,rm,rn,splop,splt) :: rest ->
        scan rest (transSpl rd rm rn splop splt :: acc)
    | Iformat.Alu2fF(opc,prec,rd,rn) :: rest ->
        scan rest (transAlu2f opc prec rd rn :: acc)
    | Iformat.Alu2fiF(opc,prec,rd,imm) :: rest ->
        scan rest (transAlu2fi opc prec rd imm :: acc)
    | Iformat.Alu3fF(opc,prec,rd,rm,rn) :: rest ->
        scan rest (transAlu3f opc prec rd rm rn :: acc)
    | Iformat.Alu3fiF(opc,prec,rd,rm,imm) :: rest ->
        scan rest (transAlu3fi opc prec rd rm imm :: acc)
    | Iformat.LdfF(rd,rb,ri,vol,prec) :: rest ->
        scan rest (transLdf rd rb ri vol prec :: acc)
    | Iformat.LdfiF(rd,rb,offset,vol,prec) :: rest ->
        scan rest (transLdfi rd rb offset vol prec :: acc)
    | Iformat.StfF(rd,rb,ri,vol,prec) :: rest ->
        scan rest (transStf rd rb ri vol prec :: acc)
    | Iformat.StfiF(rd,rb,offset,vol,prec) :: rest ->
        scan rest (transStfi rd rb offset vol prec :: acc)
    | Iformat.LdmfF(rb,dra,up,wb) :: rest ->
        scan rest (transLdmf rb dra up wb acc)
    | Iformat.StmfF(rb,sra,up,wb) :: rest ->
        scan rest (transStmf rb sra up wb acc)
    | Iformat.FixF(prec,rd,rs) :: rest ->
        scan rest (transFix prec rd rs :: acc)
    | Iformat.FltF(prec,rd,rs) :: rest ->
        scan rest (transFlt prec rd rs :: acc)
    | Iformat.LdtF(opc, imm) :: rest ->
        scan rest (transLdt opc imm :: acc)
    | Iformat.TrapF(tcond,rc,loc) :: rest ->
        scan rest (transTrap tcond rc loc :: acc)
    | Iformat.SwiF(code) :: rest ->
        scan rest (transSwi code :: acc)
    | Iformat.CmpfF(_,_,_,_,_) :: rest -> failwith "CmpfF"
    | Iformat.NoopF :: rest -> failwith "NoopF"
  in
    scan ilist []

(* let transblock state blocknum =
  let reglocs = Array.make 64 Initial in
  let rec makelist addr linenum acc =
    try
      makelist (addr +! !!4) (succ linenum)
        ((translate reglocs (Decode.decode (Memory.readWord state addr)) 
          linenum) :: acc)
    with Done(term) -> Block.Block(acc, term)
  in
    makelist (Lookup.lookup blocknum state) 0 *)

(* let translist ilist =
  let rec transitem ilist accu =
    try
      begin match ilist with
        [] -> raise NoTerminator
      | l::ls -> transitem ls (translate l :: accu)
      end
    with
      Done(term) -> Block.Block(accu,term)
  in
    transitem ilist []
*)

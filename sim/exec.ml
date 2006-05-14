open State
open Memory
open Common
open Iformat
open I32op

exception BadFormat
exception NotImplemented

let readreg proc rn =
  match rn with
    Reg(rn, _) -> proc#getreg rn

let writereg proc rn v =
  match rn with
    Dreg(rn) -> proc#putreg rn v

let readfreg proc rn =
  match rn with
    Freg(rn, _) -> proc#getfreg rn

let writefreg proc rn v =
  match rn with
    Dfreg(rn) -> proc#putfreg rn v

let srctodest reg =
  let Reg(num, _) = reg in Dreg(num)

(* whatever 'dest' reg we're using, we will overwrite it immediately *)
let desttosrc reg =
  let Dreg(num) = reg in Reg(num, Dead)

let execAlu2 proc opc rd op2 =
  let v =
    begin match opc with
      Mov -> op2
    | Not -> Int32.lognot op2
    end
  in
    writereg proc rd v; true

let execAlu3 proc opc rd op1 op2 =
  let v =
    begin match opc with
      Lsl -> Int32.shift_left op1 (Int32.to_int op2)
    | Lsr -> Int32.shift_right_logical op1 (Int32.to_int op2)
    | Asr -> Int32.shift_right op1 (Int32.to_int op2)
    | Ror ->
        let amt = (Int32.to_int op2)
        in
          Int32.logor (Int32.shift_right_logical op1 amt)
                      (Int32.shift_left op1 (32-amt))
    | And -> Int32.logand op1 op2
    | Ior -> Int32.logor op1 op2
    | Eor -> Int32.logxor op1 op2
    | Bic -> Int32.logand op1 (Int32.lognot op2)
    | Add -> Int32.add op1 op2
    | Sub -> Int32.sub op1 op2
    | Rsb -> Int32.sub op2 op1
    | Mul -> Int32.mul op1 op2
    | Div -> Int32.div op1 op2
    | Udiv -> Common.uDiv op1 op2
    | Mod -> Int32.rem op1 op2
    | Umod -> Common.uRem op1 op2
    end;
  in
    writereg proc rd v; true

let execMvc state rd imm fill replace part =
  let src =
    match replace with
      Replace ->
        begin match fill with
          Ones -> Int32.minus_one
        | Zeros -> Int32.zero
        end
    | Merge -> readreg state (desttosrc rd)
  in
    let dest =
      match part with
        Hi -> insertbits src 16 31 imm
      | Lo -> insertbits src 0 15 imm
    in
      writereg state rd dest; true

let condrep b = if b then Int32.minus_one else Int32.zero

let condtruth x =
  if x=Int32.zero then false else true

(* Make unsigned greater-than comparison out of signed comparison primitives.
   Hideously inefficient.
   --new-- best 30 quid I ever spent, maybe.
*)

let bias = 0x80000000l

let uGt a b =
  (Int32.sub a bias) > (Int32.sub b bias)

let uGe a b =
  (Int32.sub a bias) >= (Int32.sub b bias)

let uLt a b =
  (Int32.sub a bias) < (Int32.sub b bias)

let uLe a b =
  (Int32.sub a bias) <= (Int32.sub b bias)

let cmpGuts cond op1 op2 =
  match cond with
    Eq -> condrep (op1 = op2)
  | Ne -> condrep (op1 <> op2)
  | Ge -> condrep (op1 >= op2)
  | Geu -> condrep (uGe op1 op2)
  | Gt -> condrep (op1 > op2)
  | Gtu -> condrep (uGt op1 op2)
  | Le -> condrep (op1 <= op2)
  | Leu -> condrep (uLe op1 op2)
  | Lt -> condrep (op1 < op2)
  | Ltu -> condrep (uLt op1 op2)
  | Andl -> condrep ((Int32.logand op1 op2) <> Int32.zero)
  | Eorl -> condrep ((Int32.logxor op1 op2) <> Int32.zero)
  | Nandl -> condrep ((Int32.logand op1 op2) = Int32.zero)
  | Neorl -> condrep ((Int32.logxor op1 op2) = Int32.zero)

let execCmp state cond rd op1 op2 =
  let v = cmpGuts cond op1 op2 in
    writereg state rd v; true

exception BadlyFormedProgram

let transition mach oldblk destblk =
  let proc = mach#getproc
  and mem = mach#getmem in
  if mach#getserv#getprofiler#count oldblk destblk then begin
    Disassemble.dissblock stderr mach oldblk;
    Disassemble.dissblock stderr mach destblk;
    Driver.common_transition mach oldblk destblk
(*    let tag,data,translated =
      Driver.common_transition mach oldblk destblk
    in
    begin match tag with
      Some t ->
        begin match t with
          Tag.Tag_trap_converted
        | Tag.Tag_jump_folded ->
            let tsize = Int32.of_int (4*(List.length translated)) in
            let newbase = mach#getserv#getallocator#allocate tsize in
            let baseminusprogseg = Int32.sub newbase proc#getprogseg in
            let upgraded_block_no = oldblk /! 16l in
            Driver.writelist mem newbase translated;
            Replace.upgrade mach upgraded_block_no baseminusprogseg;
            Tag.put_metadata mach upgraded_block_no t data;
            Disassemble.dissblock stderr mach oldblk;
            mach#getserv#bumpopt
        | _ -> ()
        end
    | None -> ()
    end;*)
  end

let flowcontrol trans mach =
  let proc = mach#getproc
  and mem = mach#getmem in
  let oldblk = proc#getreg 55 in
  let (tx,ty,tz) = (proc#getx, proc#gety, proc#getz) in
  let destblk = begin match trans with
    `cbr rc ->
      if (proc#getreg rc) <> Int32.zero then begin
        mach#getserv#getprofiler#count_cbr oldblk Ast.X;
        tx
      end else begin
        mach#getserv#getprofiler#count_cbr oldblk Ast.Y;
        ty
      end
  | `call -> proc#putreg 54 ty; tx
  | `jump -> tx
  | `ret -> proc#getreg 54
  | `trap -> tz
  | `stop -> raise BadlyFormedProgram
  end in
  proc#putreg 55 destblk;
  let lookedup = Lookup.lookup mem proc destblk in
    proc#putpc lookedup;
    if mach#getserv#testoptok then
      transition mach oldblk destblk;
    proc#getblktrace#put ((Int32.to_string destblk) ^ "\n")
(*    Client.grindblock state destblk *)

let execTrap mach tcond rc loc =
  let proc = mach#getproc in
  begin match loc with
    Bits(zpart) ->
      proc#putz (insertbits proc#getz 26 31 (Int32.of_int zpart))
  | Srcreg(rs) ->
      proc#putz (readreg proc rs)
  end;
  let dotrap = match tcond with
    OnZero -> rc = 0l
  | OnNonzero -> rc <> 0l
  in
    if dotrap then begin
      flowcontrol `trap mach; proc#pipeflush; false
    end else true

let execStr mem num base index vol width =
  let address = Int32.add base index in
    begin match width with
      Byte -> mem#writeByte address num
    | Halfword -> mem#writeHalfword address num
    | Word -> mem#writeWord address num
    end; true

let execLdr mach rd base index vol width =
  let mem = mach#getmem in
  let address = Int32.add base index in
    let result =
      match width with
        Byte -> mem#readByte address
      | Halfword -> mem#readHalfword address
      | Word -> mem#readWord address
    in
      writereg mach#getproc rd result; true

let execStf mem num base index vol prec =
  let address = Int32.add base index in
    begin match prec with
      Single -> mem#writeWord address (Floatsupport.float_to_int32 num)
    | Double ->
        mem#writeWord address (Floatsupport.double_to_int32_0 num);
        mem#writeWord (Int32.add address 4l)
                      (Floatsupport.double_to_int32_1 num)
    end; true

let execLdf mach rd base index vol prec =
  let mem = mach#getmem in
  let address = Int32.add base index in
    let result =
      match prec with
        Single -> Floatsupport.float_from_int32 (mem#readWord address)
      | Double ->
          Floatsupport.double_from_int32s
            (mem#readWord address)
            (mem#readWord (Int32.add address 4l))
    in
      writefreg mach#getproc rd result; true

let execBfx state rd from lo hi sex =
  let dosex = match sex with Signext -> true | Zeroext -> false in
  let extractval = extractbits from lo hi dosex in
    writereg state rd extractval; true

let execSpl state rd rm rn splop splpt =
  let mask = Int32.shift_left Int32.minus_one splpt in
  let imask = Int32.lognot mask in
  let rhs =
    match splop with
      SpliceShift -> Int32.shift_left rn splpt
    | Splice -> rn in
  let new_rd = Int32.logor (Int32.logand rm imask) (Int32.logand rhs mask) in
  writereg state rd new_rd; true

let execMmem mach rb arr stoff update writebk exec =
  let proc = mach#getproc in
  let base = readreg proc (desttosrc rb) in
  let addr = ref (Int32.add base stoff) in
  let num = Array.length arr in
  for count = 0 to num-1 do
    exec ~address:!addr ~regs:arr ~idx:count;
    addr := Int32.add !addr update;
  done;
  begin match writebk with
    Writeback -> writereg proc rb
      (Int32.add base (Int32.mul (Int32.of_int num) update))
  | Discard -> ()
  end;
  true

let execLdm mach rb dra up wb =
  let mem = mach#getmem in
  let load ~address:addr ~regs ~idx =
    writereg mach#getproc regs.(idx) (mem#readWord addr)
  in
    let start,offset = match up with
      IncAfter -> 0l, 4l
    | DecBefore -> -4l, -4l
    in
      execMmem mach rb dra start offset wb load

let execStm mach rb sra up wb =
  let mem = mach#getmem in
  let store ~address:addr ~regs ~idx =
    mem#writeWord addr (readreg mach#getproc regs.(idx))
  in
    let start,offset = match up with
      IncAfter -> 0l, 4l
    | DecBefore -> -4l, -4l
    in
      execMmem mach rb sra start offset wb store

let execLdmf mach rb dra up wb =
  let mem = mach#getmem in
  let loadf ~address:addr ~regs ~idx =
    writefreg mach#getproc regs.(idx)
      (Floatsupport.double_from_int32s
        (mem#readWord addr)
        (mem#readWord (Int32.add addr 4l)))
  in
    let start,offset = match up with
      IncAfter -> 0l, 8l
    | DecBefore -> -8l, -8l
    in
      execMmem mach rb dra start offset wb loadf

let execStmf mach rb sra up wb =
  let mem = mach#getmem in
  let storef ~address:addr ~regs ~idx =
    let regval = readfreg mach#getproc regs.(idx)
    in
      mem#writeWord addr (Floatsupport.double_to_int32_0 regval);
      mem#writeWord (Int32.add addr 4l) (Floatsupport.double_to_int32_1 regval)
  in
    let start,offset = match up with
      IncAfter -> 0l, 8l
    | DecBefore -> -8l, -8l
    in
      execMmem mach rb sra start offset wb storef

let execAlu2f proc opc prec rd op =
  let res = match prec with
    Single ->
      begin match opc with
        Movf -> Floatsupport.float_mov op
      | Negf -> Floatsupport.float_neg op
      | Absf -> Floatsupport.float_abs op
      | Sqrf -> Floatsupport.float_sqr op
      end
  | Double ->
      begin match opc with
        Movf -> op
      | Negf -> -. op
      | Absf -> abs_float op
      | Sqrf -> sqrt op
      end
  in
    writefreg proc rd res; true

let execAlu3f proc opc prec rd op1 op2 =
  let res = match prec with
    Single ->
      begin match opc with
        Addf -> Floatsupport.float_add op1 op2
      | Subf -> Floatsupport.float_sub op1 op2
      | Mulf -> Floatsupport.float_mul op1 op2
      | Divf -> Floatsupport.float_div op1 op2
      end
  | Double ->
      begin match opc with
        Addf -> op1 +. op2
      | Subf -> op1 -. op2
      | Mulf -> op1 *. op2
      | Divf -> op1 /. op2
      end
  in
    writefreg proc rd res; true

let execFix state prec rd op =
  let res = match prec with
    Single -> Floatsupport.float_fix op
  | Double -> Int32.of_float op
  in
    writereg state rd res; true

let execFlt state prec rd op =
  let res = match prec with
    Single -> Floatsupport.float_flt op
  | Double -> Int32.to_float op
  in
    writefreg state rd res; true

exception UnsignedFloatComparison

let execCmpf state cond prec rc op1 op2 =
  let res = match prec with
    Single ->
      begin match cond with
        Eq -> Floatsupport.float_eq op1 op2
      | Ne -> Int32.logxor (Floatsupport.float_eq op1 op2) Int32.minus_one
      | Le -> Floatsupport.float_le op1 op2
      | Ge -> Floatsupport.float_le op2 op1
      | Lt -> Floatsupport.float_lt op1 op2
      | Gt -> Floatsupport.float_lt op2 op1
      | _ -> raise UnsignedFloatComparison
      end
  | Double ->
      begin match cond with
        Eq -> condrep (op1 = op2)
      | Ne -> condrep (op1 <> op2)
      | Le -> condrep (op1 <= op2)
      | Lt -> condrep (op1 < op2)
      | Ge -> condrep (op1 >= op2)
      | Gt -> condrep (op1 > op2)
      | _ -> raise UnsignedFloatComparison
      end
  in
    writereg state rc res; true

(* This is 'sort of' part of the following Tfer instruction *)
let execLdt proc opc imm =
  begin match opc with
    Ldx -> proc#putx imm
  | Ldy -> proc#puty imm
  | Ldz -> proc#putz imm
  end; true

let nop = Iformat.NoopF

(* let pipeflush state =
  ignore (state.dlatch nop);
  ignore (state.flatch (Int32.shift_left 60l 26)) *)

(* This is the delayed-jump version *)
(*let execTfer0 instr state =
  match instr with
    TferF(opc, Reg(rc), Reg(rx), Reg(ry), ix, iy) ->
      let tx =
        if ix then insertbits state.x_reg 26 31 (Int32.of_int rx)
              else state.registers.(rx)
      and ty =
        if iy then insertbits state.y_reg 26 31 (Int32.of_int ry)
              else state.registers.(ry)
      in
        let retcode = begin match opc with
          Cbr ->
            state.xfertype <- Xcbr;
            state.c_reg <- rc; true
        | Call ->
            state.xfertype <- Xcall;
            state.c_reg <- rc; true
        | Jump ->
            state.xfertype <- Xjump;
            state.c_reg <- rc; true
        | Ret ->
            state.xfertype <- Xret;
            state.c_reg <- rc; true
        | Stop ->
            flowcontrol state.xfertype state.c_reg state;
            pipeflush state; false
        | _ -> raise BadFormat
        end in
        state.x_reg <- tx;
        state.y_reg <- ty;
        retcode
   | _ -> raise BadFormat *)

(* This is the immediate-jump version *)
(*let execTfer instr state =
  match instr with
    TferF(opc, Reg(rc), Reg(rx), Reg(ry), ix, iy) ->
      let tx =
        if ix then insertbits state.x_reg 26 31 (Int32.of_int rx)
              else state.registers.(rx)
      and ty =
        if iy then insertbits state.y_reg 26 31 (Int32.of_int ry)
              else state.registers.(ry)
      in
        state.x_reg <- tx;
        state.y_reg <- ty;
        let retcode = begin
          match opc with
            Cbr ->
              flowcontrol Xcbr rc state; pipeflush state; false
          | Call ->
              flowcontrol Xcall rc state; pipeflush state; false
          | Jump ->
              flowcontrol Xjump rc state; pipeflush state; false
          | Ret ->
              flowcontrol Xret rc state; pipeflush state; false
          | Stop -> raise BadFormat
          | _ -> raise BadFormat
          end
        in
          retcode
   | _ -> raise BadFormat *)

let mergectrl proc bits ireg =
  match ireg with
    Bits(xpart) -> insertbits bits 26 31 (Int32.of_int xpart)
  | Srcreg(r) -> readreg proc r

let execCbr mach rc tru fal =
  let proc = mach#getproc in
  proc#putx (mergectrl proc proc#getx tru);
  proc#puty (mergectrl proc proc#gety fal);
  let Reg(rc,_) = rc in flowcontrol (`cbr rc) mach; proc#pipeflush; false

let execCall mach dest link =
  let proc = mach#getproc in
  proc#putx (mergectrl proc proc#getx dest);
  proc#puty (mergectrl proc proc#gety link);
  flowcontrol `call mach; proc#pipeflush; false

let execJump mach dest =
  let proc = mach#getproc in
  proc#putx (mergectrl proc proc#getx dest);
  flowcontrol `jump mach; proc#pipeflush; false

let execRet mach =
  let proc = mach#getproc in
  flowcontrol `ret mach; proc#pipeflush; false

let execSwi mach num =
  Syscalls.dispatch (Int32.to_int num) mach;
  true

let exec mach instr =
  let proc = mach#getproc
  and mem = mach#getmem in
  let readreg = readreg proc
  and readfreg = readfreg proc in
  let success =
    match instr with
      Alu2F(opc, rd, rn) ->
        execAlu2 proc opc rd (readreg rn)
    | Alu2iF(opc, rd, imm) ->
        execAlu2 proc opc rd imm
    | Alu3F(opc, rd, rm, rn) ->
        execAlu3 proc opc rd (readreg rm) (readreg rn)
    | Alu3iF(opc, rd, rm, imm) ->
        execAlu3 proc opc rd (readreg rm) imm
    | Alu2fF(opc, prec, rd, rn) ->
        execAlu2f proc opc prec rd (readfreg rn)
    | Alu2fiF(opc, prec, rd, imm) ->
        execAlu2f proc opc prec rd (Int32.to_float imm)
    | Alu3fF(opc, prec, rd, rm, rn) ->
        execAlu3f proc opc prec rd (readfreg rm) (readfreg rn)
    | Alu3fiF(opc, prec, rd, rm, imm) ->
        execAlu3f proc opc prec rd (readfreg rm) (Int32.to_float imm)
    | FixF(prec, rd, rm) ->
        execFix proc prec rd (readfreg rm)
    | FltF(prec, rd, rm) ->
        execFlt proc prec rd (readreg rm)
    | MvcF(rd, imm, fill, replace, part) ->
        execMvc proc rd imm fill replace part
    | CmpF(cond, rd, rm, rn) ->
        execCmp proc cond rd (readreg rm) (readreg rn)
    | CmpiF(cond, rd, rm, imm) ->
        execCmp proc cond rd (readreg rm) imm
    | CmpfF(cond, prec, rc, rm, rn) ->
        execCmpf proc cond prec rc (readfreg rm) (readfreg rn)
    | LdrF(rd, rb, ri, vol, width) ->
        execLdr mach rd (readreg rb) (readreg ri) vol width
    | LdriF(rd, rb, offset, vol, width) ->
        execLdr mach rd (readreg rb) offset vol width
    | StrF(rd, rb, ri, vol, width) ->
        execStr mem (readreg rd) (readreg rb) (readreg ri) vol width
    | StriF(rd, rb, offset, vol, width) ->
        execStr mem (readreg rd) (readreg rb) offset vol width
    | LdfF(rd, rb, ri, vol, prec) ->
        execLdf mach rd (readreg rb) (readreg ri) vol prec
    | LdfiF(rd, rb, offset, vol, prec) ->
        execLdf mach rd (readreg rb) offset vol prec
    | StfF(rd, rb, ri, vol, prec) ->
        execStf mem (readfreg rd) (readreg rb) (readreg ri) vol prec
    | StfiF(rd, rb, offset, vol, prec) ->
        execStf mem (readfreg rd) (readreg rb) offset vol prec
    | BfxF(rd, rm, lo, hi, sex) ->
        execBfx proc rd (readreg rm) lo hi sex
    | SplF(rd, rm, rn, splop, splpt) ->
        execSpl proc rd (readreg rm) (readreg rn) splop splpt
    | LdmF(rb, dra, up, wb) ->
        execLdm mach rb dra up wb
    | StmF(rb, sra, up, wb) ->
        execStm mach rb sra up wb
    | LdmfF(rb, dra, up, wb) ->
        execLdmf mach rb dra up wb
    | StmfF(rb, sra, up, wb) ->
        execStmf mach rb sra up wb
    | LdtF(opc, imm) ->
        execLdt proc opc imm
    | TrapF(tcond, rc, loc) ->
        execTrap mach tcond (readreg rc) loc
    | CbrF(rc, tru, fal) ->
        execCbr mach rc tru fal
    | CallF(dest, link) ->
        execCall mach dest link
    | JumpF(dest) ->
        execJump mach dest
    | RetF ->
        execRet mach
    | SwiF(imm) ->
        execSwi mach imm
    | NoopF -> true
  in
    if success then proc#incrpc

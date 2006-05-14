open Common
open Int32
open I32op

type aluopcode2 = Mov | Not

and aluopcode3 = Lsl | Lsr | Asr | Ror | And | Ior | Eor | Bic | Add |
                 Sub | Rsb | Mul | Div | Udiv | Mod | Umod | Mlh | Umlh

and alufopcode2 = Movf | Negf | Absf | Sqrf

and alufopcode3 = Addf | Subf | Mulf | Divf

and mmemfopcode = Ldmf | Stmf

(* and trapopcode = Trap | Itrap | Trapi | Itrapi | Utrap | Uitrap | Utrapi |
                 Uitrapi *)

and cond = Eq | Ne | Ge | Geu | Gt | Gtu | Le | Leu | Lt | Ltu |
           Andl | Eorl | Nandl | Neorl

and tcond = OnZero | OnNonzero

and ldtopcode = Ldx | Ldy | Ldz

and width = Byte | Halfword | Word

and life = Alive | Dead

and isimm = Immediate | Indirect

and srcreg = Reg of int * life

and srcfreg = Freg of int * life

and destreg = Dreg of int

and destfreg = Dfreg of int

and immreg = Bits of int | Srcreg of srcreg

and replace = Replace | Merge

and fill = Ones | Zeros

and extend = Signext | Zeroext

and part = Hi | Lo

and volatile = Volatile | Nonvolatile

and precision = Single | Double

and up = IncAfter | DecBefore

and writebk = Writeback | Discard

and shft = SpliceShift | Splice

and format = Alu2F of aluopcode2 * destreg * srcreg
           | Alu2iF of aluopcode2 * destreg * int32
           | Alu3F of aluopcode3 * destreg * srcreg * srcreg
           | Alu3iF of aluopcode3 * destreg * srcreg * int32
           | Alu2fF of alufopcode2 * precision * destfreg * srcfreg
           | Alu2fiF of alufopcode2 * precision * destfreg * int32
           | Alu3fF of alufopcode3 * precision * destfreg * srcfreg * srcfreg
           | Alu3fiF of alufopcode3 * precision * destfreg * srcfreg * int32
           | FltF of precision * destfreg * srcreg
           | FixF of precision * destreg * srcfreg
           | CmpF of cond * destreg * srcreg * srcreg
           | CmpiF of cond * destreg * srcreg * int32
           | CmpfF of cond * precision * destreg * srcfreg * srcfreg
           | MvcF of destreg * int32 * fill * replace * part
           | LdrF of destreg * srcreg * srcreg * volatile * width
           | LdriF of destreg * srcreg * int32 * volatile * width
           | StrF of srcreg * srcreg * srcreg * volatile * width
           | StriF of srcreg * srcreg * int32 * volatile * width
           | LdfF of destfreg * srcreg * srcreg * volatile * precision
           | LdfiF of destfreg * srcreg * int32 * volatile * precision
           | StfF of srcfreg * srcreg * srcreg * volatile * precision
           | StfiF of srcfreg * srcreg * int32 * volatile * precision
           | BfxF of destreg * srcreg * int * int * extend
           | SplF of destreg * srcreg * srcreg * shft * int
           | LdmF of destreg * destreg array * up * writebk
           | StmF of destreg * srcreg array * up * writebk
           | LdmfF of destreg * destfreg array * up * writebk
           | StmfF of destreg * srcfreg array * up * writebk
           | SwiF of int32
           | LdtF of ldtopcode * int32
           | TrapF of tcond * srcreg * immreg
           | CbrF of srcreg * immreg * immreg
           | CallF of immreg * immreg
           | JumpF of immreg
           | RetF
           | NoopF

let decodemb inst lowbit =
  let mantissa = bits inst lowbit (lowbit+7)
  and bytenum = bits inst (lowbit+8) (lowbit+9) in
  shift_left mantissa (8*(to_int bytenum))
  
exception BadBits

(*let regbits inst lo hi =
  if (hi-lo)==5 then
    Reg(to_int (bits inst lo hi))
  else
    raise BadBits *)

let dr inst lowbit =
  Dreg((to_int (shift_right_logical inst lowbit)) land 63)

let exexp inst bit =
  if truth inst bit then Dead else Alive

let sr inst lowbit expire =
  Reg((to_int (shift_right_logical inst lowbit)) land 63, exexp inst expire)

let ir inst lowbit immediate expire =
  let num = (to_int (shift_right_logical inst lowbit)) land 63 in
  if truth inst immediate then
    Bits(num)
  else
    Srcreg(Reg(num, if truth inst expire then Dead else Alive))

let ir2 inst lowbit immediate expire =
  let num = (to_int (shift_right_logical inst lowbit)) land 63 in
  match immediate with
    Immediate -> Bits(num)
  | Indirect -> Srcreg(Reg(num, if truth inst expire then Dead else Alive))

let drf inst lowbit =
  Dfreg((to_int (shift_right_logical inst lowbit)) land 63)

let srf inst lowbit expire =
  Freg((to_int (shift_right_logical inst lowbit)) land 63,
       if truth inst expire then Dead else Alive)

let decodeAlu2 opc inst =
  if truth inst 25 then
    Alu2iF(opc, dr inst 0, decodemb inst 12)
  else
    Alu2F(opc, dr inst 0, sr inst 12 23)

let decodeAlu3 opc inst =
  if truth inst 25 then
    Alu3iF(opc, dr inst 0, sr inst 6 24, decodemb inst 12)
  else
    Alu3F(opc, dr inst 0, sr inst 6 24, sr inst 12 23)

let prec inst lowbit =
  let num = (to_int (shift_right_logical inst lowbit)) land 3 in
  [| Single; Double |].(num)

let decodeAluf2 opc inst =
  if truth inst 25 then
    Alu2fiF(opc, prec inst 22, drf inst 0, decodemb inst 12)
  else
    Alu2fF(opc, prec inst 21, drf inst 0, srf inst 12 23)

let decodeAluf3 opc inst =
  if truth inst 25 then
    Alu3fiF(opc, prec inst 22, drf inst 0, srf inst 6 24, decodemb inst 12)
  else
    Alu3fF(opc, prec inst 21, drf inst 0, srf inst 6 24, srf inst 12 23)

let decodeFixf inst =
  FixF(prec inst 21, dr inst 0, srf inst 12 23)

let decodeFltf inst =
  FltF(prec inst 21, drf inst 0, sr inst 12 23)

let scond inst lowbit =
  let num = (to_int (shift_right_logical inst lowbit)) land 7 in
  [| Eq; Ne; Ge; Gt; Le; Lt; Andl; Eorl |].(num)

let ucond inst lowbit =
  let num = (to_int (shift_right_logical inst lowbit)) land 7 in
  [| Eq; Ne; Geu; Gtu; Leu; Ltu; Nandl; Neorl |].(num)

let decodeCmp ctype inst =
  CmpF(ctype inst 22, dr inst 0, sr inst 6 25, sr inst 12 21)

let decodeCmpi ctype inst =
  CmpiF(ctype inst 22, dr inst 0, sr inst 6 25, decodemb inst 12)

let decodeCmpf inst =
  CmpfF(scond inst 22, prec inst 18, dr inst 0, srf inst 6 25, srf inst 12 21)

let decodeTrap tcond inst =
  TrapF(tcond, sr inst 0 21, ir inst 6 24 22)

let decodeMvc inst =
  MvcF(dr inst 0, bits inst 6 21,
       (if truth inst 23 then Ones else Zeros),
       (if truth inst 24 then Replace else Merge),
       (if truth inst 25 then Hi else Lo))

(*let exdom inst lowbit =
  Domain((to_int (shift_right_logical inst lowbit)) land 3)*)

let exvol inst lowbit =
  if truth inst lowbit then Volatile else Nonvolatile

let exwidth inst lowbit =
  let num = (to_int (shift_right_logical inst lowbit)) land 3 in
  [| Byte; Halfword; Word |].(num)

let scaledoffset inst lowbit width =
  let num = (to_int (shift_right_logical inst lowbit)) land 255 in
  let snum = if num>=128 then num-256 else num in
  let scale = match width with Byte -> 1 | Halfword -> 2 | Word -> 4 in
  of_int (snum*scale)

let scaledfloatoffset inst lowbit width =
  let num = (to_int (shift_right_logical inst lowbit)) land 255 in
  let snum = if num>=128 then num-256 else num in
  let scale = match width with Single -> 4 | Double -> 4 in
  of_int (snum*scale)

let decodeLdr inst =
  let width = exwidth inst 22
  in
    if truth inst 25 then
        LdriF(dr inst 0, sr inst 6 20, scaledoffset inst 12 width,
              exvol inst 21, width)
    else
      LdrF(dr inst 0, sr inst 6 20, sr inst 12 18, exvol inst 21, width)

let decodeStr inst =
  let width = exwidth inst 22
  in
    if truth inst 25 then
        StriF(sr inst 0 24, sr inst 6 20, scaledoffset inst 12 width,
              exvol inst 21, width)
    else
      StrF(sr inst 0 24, sr inst 6 20, sr inst 12 18, exvol inst 21, width)

let decodeLdf inst =
  let prec = prec inst 22
  in
    if truth inst 25 then
        LdfiF(drf inst 0, sr inst 6 20, scaledfloatoffset inst 12 prec,
              exvol inst 21, prec)
    else
      LdfF(drf inst 0, sr inst 6 20, sr inst 12 18, exvol inst 21, prec)

let decodeStf inst =
  let prec = prec inst 22
  in
    if truth inst 25 then
        StfiF(srf inst 0 24, sr inst 6 20, scaledfloatoffset inst 12 prec,
              exvol inst 21, prec)
    else
      StfF(srf inst 0 24, sr inst 6 20, sr inst 12 18, exvol inst 21, prec)

let decodeBfx inst =
  BfxF(dr inst 0, sr inst 6 25, to_int (bits inst 12 16),
       to_int (bits inst 17 21), if truth inst 23 then Signext else Zeroext)

let decodeSpl inst =
  SplF(dr inst 0, sr inst 6 24, sr inst 12 23,
       (if truth inst 25 then SpliceShift else Splice),
       to_int (bits inst 18 22))

let arr123 inst i li j lj k lk gen =
  let ireg = (to_int (shift_right_logical inst i)) land 63
  and jreg = (to_int (shift_right_logical inst j)) land 63
  and kreg = (to_int (shift_right_logical inst k)) land 63 in
  match ireg, jreg, kreg with
    ii, 63, 63 -> [| gen ii (exexp inst li) |]
  | ii, jj, 63 -> [| gen ii (exexp inst li);
                     gen jj (exexp inst lj) |]
  | ii, jj, kk -> [| gen ii (exexp inst li);
                     gen jj (exexp inst lj);
                     gen kk (exexp inst lk) |]

let exwb inst bit =
  if truth inst bit then Writeback else Discard

let exup inst bit =
  if truth inst bit then IncAfter else DecBefore

let dr3 inst lowbit =
  Dreg(((to_int (shift_right_logical inst lowbit)) land 7) lor 56)

let decodeLdm inst =
  LdmF(dr3 inst 18, arr123 inst 0 23 6 24 12 25 (fun q _ -> Dreg q),
       exup inst 21, exwb inst 22)

let decodeStm inst =
  StmF(dr3 inst 18, arr123 inst 0 23 6 24 12 25 (fun q x -> Reg(q,x)),
       exup inst 21, exwb inst 22)

let decodeLdmf inst =
  LdmfF(dr3 inst 18, arr123 inst 0 23 6 24 12 25 (fun q _ -> Dfreg q),
        exup inst 21, exwb inst 22)

let decodeStmf inst =
  StmfF(dr3 inst 18, arr123 inst 0 23 6 24 12 25 (fun q x -> Freg(q,x)),
        exup inst 21, exwb inst 22)

let decodeSwi inst =
  SwiF(bits inst 0 25)

let decodeLdt opc inst =
  LdtF(opc, bits inst 0 25)

let decodeCbr inst =
  CbrF(sr inst 0 21, ir inst 6 24 22, ir inst 12 25 23)

let decodeCall inst =
  CallF(ir inst 6 24 22, ir inst 12 25 23)

let decodeJump inst =
  JumpF(ir inst 6 24 22)

(* The order of this array must match format.h *)
let decodeArray =
  [| (decodeAlu2 Mov);      (*  0 *)
     (decodeAlu2 Not);
     (decodeAlu3 Lsl);
     (decodeAlu3 Lsr);
     (decodeAlu3 Asr);      (*  4 *)
     (decodeAlu3 Ror);
     (decodeAlu3 And);
     (decodeAlu3 Ior);
     (decodeAlu3 Eor);      (*  8 *)
     (decodeAlu3 Bic);
     (decodeAlu3 Add);
     (decodeAlu3 Sub);
     (decodeAlu3 Rsb);      (* 12 *)
     (decodeAlu3 Mul);
     (decodeAlu3 Div);
     (decodeAlu3 Udiv);
     (decodeAlu3 Mod);      (* 16 *)
     (decodeAlu3 Umod);
     (decodeAlu3 Mlh);
     (decodeAlu3 Umlh);
     decodeMvc;             (* 20 *)
     (decodeCmp scond);
     (decodeCmp ucond);
     (decodeCmpi scond);
     (decodeCmpi ucond);    (* 24 *)
     decodeBfx;
     decodeLdr;
     decodeStr;
     decodeLdm;             (* 28 *)
     decodeStm;
     decodeSwi;
     (decodeAluf2 Movf);
     (decodeAluf2 Negf);    (* 32 *)
     (decodeAluf2 Absf);
     (decodeAluf2 Sqrf);
     decodeFltf;
     decodeFixf;            (* 36 *)
     (decodeAluf3 Addf);
     (decodeAluf3 Subf);
     (decodeAluf3 Mulf);
     (decodeAluf3 Divf);    (* 40 *)
     decodeCmpf;
     decodeLdf;
     decodeStf;
     decodeLdmf;            (* 44 *)
     decodeStmf; 
     (decodeTrap OnZero);
     (decodeTrap OnNonzero);
     decodeCbr;             (* 48 *)
     decodeCall;
     decodeJump;
     (fun _ -> RetF);
     decodeSpl;             (* 52 *)
     (fun _ -> NoopF);
     (fun _ -> NoopF);
     (fun _ -> NoopF);
     (fun _ -> NoopF);      (* 56 *)
     (fun _ -> NoopF);
     (fun _ -> NoopF);
     (fun _ -> NoopF);
     (fun _ -> NoopF);      (* 60 *)
     (decodeLdt Ldx);
     (decodeLdt Ldy);
     (decodeLdt Ldz)
  |]


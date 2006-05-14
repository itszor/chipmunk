open State
open Common
open Iformat
open I32op

exception BadFormat

let alu2name = function
    Mov -> "mov"
  | Not -> "not"

let alu3name = function
    Lsl -> "lsl"
  | Lsr -> "lsr"
  | Asr -> "asr"
  | Ror -> "ror"
  | And -> "and"
  | Ior -> "ior"
  | Eor -> "eor"
  | Bic -> "bic"
  | Add -> "add"
  | Sub -> "sub"
  | Rsb -> "rsb"
  | Mul -> "mul"
  | Div -> "div"
  | Mod -> "mod"
  | Udiv -> "udiv"
  | Umod -> "umod"
  | Mlh -> "mlh"
  | Umlh -> "umlh"

let aluf2name = function
    Movf -> "movf"
  | Negf -> "negf"
  | Absf -> "absf"
  | Sqrf -> "sqrf"

let aluf3name = function
    Addf -> "addf"
  | Subf -> "subf"
  | Mulf -> "mulf"
  | Divf -> "divf"

let expire = function
    Alive -> ""
  | Dead -> "~"

let register r = "r" ^ (string_of_int r)

let srcreg = function
    Reg(num,life) -> (expire life) ^ (register num)

let destreg = function
    Dreg(num) -> register num

let floatreg f = "f" ^ (string_of_int f)

let srcfreg = function
    Freg(num,life) -> (expire life) ^ (floatreg num)

let destfreg = function
    Dfreg(num) -> floatreg num

let immPrint imm = "#" ^ (Int32.to_string imm)

let rec vmgetstring mach ptr =
  let mem = mach#getmem in
  match (toint (mem#readByte ptr)) with
    0 -> ""
  | n -> Char.escaped (Char.chr n) ^ vmgetstring mach ((++!) ptr)

let rec vmputstring mach ptr str =
  let mem = mach#getmem in
  for c = 0 to (String.length str)-1 do
    mem#writeByte (ptr +! !!c) !!(Char.code str.[c])
  done;
  mem#writeByte (ptr +! !!(String.length str)) Int32.zero

(* let blockname mach num =
  let proc = mach#getproc
  and mem = mach#getmem in
  let addr = proc#getindirseg +! ((num *! 16l) +! 8l) in
  let str = proc#getnameseg +! (mem#readWord addr) in
    vmgetstring mach str *)

let immreg mach reg other =
  match reg with
    Bits(part) -> Tag.blockname mach ((insertbits other 26 31 !!part) /! 16l)
  | Srcreg(sr) -> srcreg sr

let prec = function
    Single -> "s"
  | Double -> "d"

let disAlu2 state opc rd rn =
  (alu2name opc) ^ " " ^ (destreg rd) ^ ", " ^ (srcreg rn)

let disAlui2 state opc rd imm =
  (alu2name opc) ^ " " ^ (destreg rd) ^ ", " ^ (immPrint imm)

let disAlu3 state opc rd rm rn =
  (alu3name opc) ^ " " ^ (destreg rd) ^ ", " ^ (srcreg rm) ^ ", " ^ (srcreg rn)

let disAlui3 state opc rd rm imm =
  (alu3name opc) ^ " " ^ (destreg rd) ^ ", " ^ (srcreg rm) ^ ", " ^
    (immPrint imm)

let disAluf2 state opc p rd rn =
  (aluf2name opc) ^ "." ^ (prec p) ^ " " ^ (destfreg rd) ^ ", " ^ (srcfreg rn)

let disAlufi2 state opc p rd imm =
  (aluf2name opc) ^ "." ^ (prec p) ^ " " ^ (destfreg rd) ^ ", " ^ (immPrint imm)

let disAluf3 state opc p rd rm rn =
  (aluf3name opc) ^ "." ^ (prec p) ^ " " ^ (destfreg rd) ^ ", " ^
    (srcfreg rm) ^ ", " ^ (srcfreg rn)

let disAlufi3 state opc p rd rm imm =
  (aluf3name opc) ^ "." ^ (prec p) ^ " " ^ (destfreg rd) ^ ", " ^
    (srcfreg rm) ^ ", " ^ (immPrint imm)

let mvcImm imm fill replace high =
  let valstr = function Ones -> "ffff" | Zeros -> "0000" in
  let repstr v = function Replace -> (valstr v) | Merge -> "----" in
  match high with
    Hi -> (Printf.sprintf "%.4lx" (Int32.shift_right_logical imm 16)) ^
          (repstr fill replace)
  | Lo -> (repstr fill replace) ^
          (Printf.sprintf "%.4lx" (Int32.logand imm 0xffffl))

let disMvc state rd imm fill replace part =
  let lohi = function Hi -> "h" | Lo -> "l"
  and fullempty r v =
    match r with
      Replace -> begin match v with Ones -> "f" | Zeros -> "e" end
    | Merge -> ""
  in
    "mvc." ^ (fullempty replace fill) ^ (lohi part) ^ " " ^ (destreg rd)
             ^ ", #0x" ^ (mvcImm imm fill replace part)

let disFix p rd rm =
  "fixf." ^ (prec p) ^ " " ^ (destreg rd) ^ ", " ^ (srcfreg rm)

let disFlt p rd rm =
  "fltf." ^ (prec p) ^ " " ^ (destfreg rd) ^ ", " ^ (srcreg rm)

let cmpName = function
    Eq -> "eq"
  | Ne -> "ne"
  | Ge -> "ge"
  | Geu -> "geu"
  | Gt -> "gt"
  | Gtu -> "gtu"
  | Le -> "le"
  | Leu -> "leu"
  | Lt -> "lt"
  | Ltu -> "ltu"
  | Andl -> "andl"
  | Eorl -> "eorl"
  | Nandl -> "nandl"
  | Neorl -> "neorl"

let tcName = function
    OnZero -> "z"
  | OnNonzero -> "n"

let disCmp state cond rd rm rn =
  "cmp." ^ (cmpName cond) ^ " " ^ (destreg rd) ^ ", " ^ (srcreg rm) ^
               ", " ^ (srcreg rn)

let disCmpi state cond rd rm imm =
  "cmp." ^ (cmpName cond) ^ " " ^ (destreg rd) ^ ", " ^ (srcreg rm) ^
    ", " ^ (immPrint imm)

let disCmpf state cond p rd rm rn =
  "fcmp." ^ (cmpName cond) ^ "." ^ (prec p) ^ " " ^ (destreg rd) ^ ", " ^
    (srcfreg rm) ^ ", " ^ (srcfreg rn)

let width = function
    Byte -> "b"
  | Halfword -> "h"
  | Word -> "w"

(* let domain = function
  Domain(dom) -> "@" ^ (string_of_int dom) *)

let volat = function
    Volatile -> "v"
  | Nonvolatile -> ""

let disStr state rs base index vol wide =
  "str." ^ (width wide) ^ (volat vol) ^ " " ^ (srcreg rs) ^ ", [" ^
    (srcreg base) ^ ", " ^ (srcreg index) ^ "]"

let disStri state rs base offset vol wide =
  "str." ^ (width wide) ^ (volat vol) ^ " " ^ (srcreg rs) ^ ", [" ^
    (srcreg base) ^ ", " ^ (immPrint offset) ^ "]"

let disLdr state rd base index vol wide =
  "ldr." ^ (width wide) ^ (volat vol) ^ " " ^ (destreg rd) ^ ", [" ^
    (srcreg base) ^ ", " ^ (srcreg index) ^ "]"

let disLdri state rd base offset vol wide =
  "ldr." ^ (width wide) ^ (volat vol) ^ " " ^ (destreg rd) ^ ", [" ^
    (srcreg base) ^ ", " ^ (immPrint offset) ^ "]"

let disStf state rs base index vol precision =
  "stf." ^ (prec precision) ^ (volat vol) ^ " " ^ (srcfreg rs) ^ ", [" ^
    (srcreg base) ^ ", " ^ (srcreg index) ^ "]"

let disStfi state rs base offset vol precision =
  "stf." ^ (prec precision) ^ (volat vol) ^ " " ^ (srcfreg rs) ^ ", [" ^
    (srcreg base) ^ ", " ^ (immPrint offset) ^ "]"

let disLdf state rd base index vol precision =
  "ldf." ^ (prec precision) ^ (volat vol) ^ " " ^ (destfreg rd) ^ ", [" ^
    (srcreg base) ^ ", " ^ (srcreg index) ^ "]"

let disLdfi state rd base offset vol precision =
  "ldf." ^ (prec precision) ^ (volat vol) ^ " " ^ (destfreg rd) ^ ", [" ^
    (srcreg base) ^ ", " ^ (immPrint offset) ^ "]"

let disBfx state rd from lo hi signx =
  let dots = match signx with Signext -> ".s " | Zeroext -> " " in
  "bfx" ^ dots ^ (destreg rd) ^ ", " ^ (srcreg from) ^ " <" ^
    (string_of_int lo) ^ "," ^ (string_of_int hi) ^ ">"

let writeup = function
    IncAfter -> "i"
  | DecBefore -> "d"

let writewb = function
    Writeback -> "*"
  | Discard -> ""

let disMmem state up rb wb arr wfn opname =
  let elems =
    match arr with
      [| i |] -> wfn i
    | [| i; j |] -> wfn i ^ ", " ^ wfn j
    | [| i; j; k |] -> wfn i ^ ", " ^ wfn j ^ ", " ^ wfn k
    | _ -> failwith "Bad multimem array"
  in
    opname ^ "." ^ (writeup up) ^ " " ^ (destreg rb) ^ (writewb wb) ^
      ", [" ^ elems ^ "]"

let disLdm state rb dra up wb =
  disMmem state up rb wb dra destreg "ldm"

let disStm state rb sra up wb =
  disMmem state up rb wb sra srcreg "stm"

let disLdmf state rb dra up wb =
  disMmem state up rb wb dra destfreg "ldmf"

let disStmf state rb sra up wb =
  disMmem state up rb wb sra srcfreg "stmf"

let disLdt state opc imm =
  let dh = state#getserv#getdishelp in
  match opc with
    Ldx -> dh#setx imm; "ldx " ^ (immPrint imm)
  | Ldy -> dh#sety imm; "ldy " ^ (immPrint imm)
  | Ldz -> dh#setz imm; "ldz " ^ (immPrint imm)

let disTrap mach tcond rc handler =
  let dh = mach#getserv#getdishelp in
  "trap." ^ (tcName tcond) ^ " " ^ (srcreg rc) ^ ", " ^
    (immreg mach handler dh#getz)

let disCbr mach rc tru fal =
  let dh = mach#getserv#getdishelp in
  "cbr " ^ (srcreg rc) ^ ", " ^ (immreg mach tru dh#getx) ^ ", " ^
    (immreg mach fal dh#gety)

let disCall mach dest link =
  let dh = mach#getserv#getdishelp in
  "call " ^ (immreg mach dest dh#getx) ^ ", " ^ (immreg mach link dh#gety)

let disJump mach dest =
  let dh = mach#getserv#getdishelp in
  "jump " ^ (immreg mach dest dh#getx)

let disRet _ = "ret"

let disSwi state num =
  "swi " ^ immPrint num

let diss state instr =
  match instr with
    Alu2F(opc, rd, rn) ->
      disAlu2 state opc rd rn
  | Alu2iF(opc, rd, imm) ->
      disAlui2 state opc rd imm
  | Alu3F(opc, rd, rm, rn) ->
      disAlu3 state opc rd rm rn
  | Alu3iF(opc, rd, rm, imm) ->
      disAlui3 state opc rd rm imm
  | Alu2fF(opc, prec, rd, rn) ->
      disAluf2 state opc prec rd rn
  | Alu2fiF(opc, prec, rd, imm) ->
      disAlufi2 state opc prec rd imm
  | Alu3fF(opc, prec, rd, rm, rn) ->
      disAluf3 state opc prec rd rm rn
  | Alu3fiF(opc, prec, rd, rm, imm) ->
      disAlufi3 state opc prec rd rm imm
  | MvcF(rd, imm, fill, replace, part) ->
      disMvc state rd imm fill replace part
  | FixF(prec, rd, rm) ->
      disFix prec rd rm
  | FltF(prec, rd, rm) ->
      disFlt prec rd rm
  | CmpF(cond, rd, rm, rn) ->
      disCmp state cond rd rm rn
  | CmpiF(cond, rd, rm, imm) ->
      disCmpi state cond rd rm imm
  | CmpfF(cond, prec, rd, rm, rn) ->
      disCmpf state cond prec rd rm rn
  | LdrF(rd, rb, ri, vol, width) ->
      disLdr state rd rb ri vol width
  | LdriF(rd, rb, offset, vol, width) ->
      disLdri state rd rb offset vol width
  | StrF(rd, rb, ri, vol, width) ->
      disStr state rd rb ri vol width
  | StriF(rd, rb, offset, vol, width) ->
      disStri state rd rb offset vol width
  | LdfF(rd, rb, ri, vol, prec) ->
      disLdf state rd rb ri vol prec
  | LdfiF(rd, rb, offset, vol, prec) ->
      disLdfi state rd rb offset vol prec
  | StfF(rd, rb, ri, vol, prec) ->
      disStf state rd rb ri vol prec
  | StfiF(rd, rb, offset, vol, prec) ->
      disStfi state rd rb offset vol prec
  | BfxF(rd, rm, lo, hi, sex) ->
      disBfx state rd rm lo hi sex
  | LdmF(rb, dra, up, wb) ->
      disLdm state rb dra up wb
  | StmF(rb, sra, up, wb) ->
      disStm state rb sra up wb
  | LdmfF(rb, dra, up, wb) ->
      disLdmf state rb dra up wb
  | StmfF(rb, sra, up, wb) ->
      disStmf state rb sra up wb
  | LdtF(opc, imm) ->
      disLdt state opc imm
  | TrapF(tcond, rc, handler) ->
      disTrap state tcond rc handler
  | CbrF(rc, tru, fal) ->
      disCbr state rc tru fal
  | CallF(dest, link) ->
      disCall state dest link
  | JumpF(dest) ->
      disJump state dest
  | RetF ->
      disRet state
  | SwiF(imm) ->
      disSwi state imm
  | NoopF -> "nop"
  
let dissblock oc mach blockno =
  let mem = mach#getmem
  and proc = mach#getproc in
  let startaddr = Lookup.lookup mem proc blockno in
  let rec uptoterm addr =
    let raw = mem#readWord addr in
    let opcode = Int32.to_int (Int32.shift_right_logical raw 26) in
    let disstr = diss mach (Decode.decode raw) in
    Printf.fprintf oc "%.8lx: %s\n" addr disstr;
    if opcode<48 || opcode>51 then uptoterm (addr +! 4l)
  in
    uptoterm startaddr;
    Printf.fprintf oc "\n";
    flush oc

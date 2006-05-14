(* Translate 'iformat' code into binary form. *)

open Iformat
open I32op

(* just binary code. *)

type encoded = {
  writeidx : int32;
  binary : int32 list
}

let alu2op code =
  let i = match code with
    Mov -> 0
  | Not -> 1
  in
    !!i

let alu3op code =
  let i = match code with
    Lsl -> 2
  | Lsr -> 3
  | Asr -> 4
  | Ror -> 5
  | And -> 6
  | Ior -> 7
  | Eor -> 8
  | Bic -> 9
  | Add -> 10
  | Sub -> 11
  | Rsb -> 12
  | Mul -> 13
  | Div -> 14
  | Udiv -> 15
  | Mod -> 16
  | Umod -> 17
  | Mlh -> 18
  | Umlh -> 19
  in
    !!i

let alu2fop code =
  let i = match code with
    Movf -> 31
  | Negf -> 32
  | Absf -> 33
  | Sqrf -> 34
  in
    !!i

let alu3fop code =
  let i = match code with
    Addf -> 37
  | Subf -> 38
  | Mulf -> 39
  | Divf -> 40
  in
    !!i

exception NotARegisterAllocator

let destreg = function
  Dreg(n) -> if n<64 then !!n else raise NotARegisterAllocator

let destfreg = function
  Dfreg(n) -> if n<64 then !!n else raise NotARegisterAllocator

let srcreg = function
  Reg(n,life) -> if n<64 then !!n else raise NotARegisterAllocator

let srcfreg = function
  Freg(n,life) -> if n<64 then !!n else raise NotARegisterAllocator

let srcexpire = function
  Reg(n,life) -> match life with Alive -> 0l | Dead -> 1l

let srfexpire = function
  Freg(n,life) -> match life with Alive -> 0l | Dead -> 1l

let mantissa v =
  match v with
    x when Int32.logand x 0xffffff00l = 0l -> v
  | x when Int32.logand x 0xffff00ffl = 0l ->
      Int32.shift_right_logical v 8 
  | x when Int32.logand x 0xff00ffffl = 0l ->
      Int32.shift_right_logical v 16
  | x when Int32.logand x 0x00ffffffl = 0l ->
      Int32.shift_right_logical v 24
  | _ -> 0l

let bytenum v =
  match v with
    x when Int32.logand x 0xffffff00l = 0l -> 0l
  | x when Int32.logand x 0xffff00ffl = 0l -> 1l
  | x when Int32.logand x 0xff00ffffl = 0l -> 2l
  | x when Int32.logand x 0x00ffffffl = 0l -> 3l
  | _ -> 0l

let fillbit = function
    Ones -> 1l
  | Zeros -> 0l

let replbit = function
    Replace -> 1l
  | Merge -> 0l

let partbit = function
    Hi -> 1l
  | Lo -> 0l

let immshift part imm =
  match part with
    Hi -> Int32.shift_right_logical imm 16
  | Lo -> Int32.logand imm 0xffffl

let cond c =
  let i,v = match c with
    Eq -> 21,0
  | Ne -> 21,1
  | Ge -> 21,2
  | Gt -> 21,3
  | Le -> 21,4
  | Lt -> 21,5
  | Andl -> 21,6
  | Eorl -> 21,7
  | Geu -> 22,2
  | Gtu -> 22,3
  | Leu -> 22,4
  | Ltu -> 22,5
  | Nandl -> 22,6
  | Neorl -> 22,7
  in
    !!i,!!v

let condopc c =
  let i,c = cond c in i

let cndiopc c =
  let i,c = cond c in i +! 2l

exception Mumble

let immreg = function
    Bits n -> !!n
  | Srcreg(Reg(n,_)) -> !!n

let lastir = function
    Bits _ -> 0l
  | Srcreg(Reg(_,Alive)) -> 0l
  | Srcreg(Reg(_,Dead)) -> 1l

let immed = function
    Bits _ -> 1l
  | Srcreg _ -> 0l

let condcc c =
  let i,c = cond c in c

let sext = function
    Signext -> 1l
  | Zeroext -> 0l

let width = function
    Byte -> 0l
  | Halfword -> 1l
  | Word -> 2l

let precn = function
    Single -> 0l
  | Double -> 1l

let offset w imm =
  match w with
    Byte -> Int32.logand imm 127l
  | Halfword -> Int32.logand (imm /! 2l) 127l
  | Word -> Int32.logand (imm /! 4l) 127l

let volatile = function
    Volatile -> 1l
  | Nonvolatile -> 0l

let dir = function
    IncAfter -> 1l
  | DecBefore -> 0l

let writebk = function
    Writeback -> 1l
  | Discard -> 0l

let mreg a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Reg(num,_) = a.(i) in !!num

let mexp a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Reg(_,life) = a.(i) in match life with Alive -> 0l | Dead -> 1l

let mdreg a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Dreg(num) = a.(i) in !!num

let mfreg a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Freg(num,_) = a.(i) in !!num

let mfexp a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Freg(_,life) = a.(i) in match life with Alive -> 0l | Dead -> 1l

let mdfreg a i =
  let l = Array.length a in
  if i>=l then
    63l
  else
    let Dfreg(num) = a.(i) in !!num

let dreg3 num =
  let Dreg(d) = num in
  !!(d land 7)

let ldtopc = function
    Ldx -> 61l
  | Ldy -> 62l
  | Ldz -> 63l

let endpt = 0l,0

let (@@) (a,b) (c,d) =
  Int32.logor a (Int32.shift_left c b), b+d

exception UnimplementedOpcode of Iformat.format

let encode e =
  let res,_ = match e with
    Alu2F(opc,dr,sr) ->
      endpt @@
      (destreg dr,   6) @@
      (0l,           6) @@
      (srcreg sr,    6) @@
      (0l,           5) @@
      (srcexpire sr, 1) @@
      (0l,           2) @@
      (alu2op opc,   6)
  | Alu2iF(opc,dr,imm) ->
      endpt @@
      (destreg dr,   6) @@
      (0l,           6) @@
      (mantissa imm, 8) @@
      (bytenum imm,  2) @@
      (0l,           3) @@
      (1l,           1) @@
      (alu2op opc,   6)
  | Alu3F(opc,rd,rm,rn) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rm,    6) @@
      (srcreg rn,    6) @@
      (0l,           5) @@
      (srcexpire rn, 1) @@
      (srcexpire rm, 1) @@
      (0l,           1) @@
      (alu3op opc,   6)
  | Alu3iF(opc,rd,rm,imm) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rm,    6) @@
      (mantissa imm, 8) @@
      (bytenum imm,  2) @@
      (0l,           2) @@
      (srcexpire rm, 1) @@
      (1l,           1) @@
      (alu3op opc,   6)
  | Alu2fF(opc,prec,fd,fs) ->
      endpt @@
      (destfreg fd,  6) @@
      (0l,           6) @@
      (srcfreg fs,   6) @@
      (0l,           3) @@
      (precn prec,   2) @@
      (srfexpire fs, 1) @@
      (0l,           2) @@
      (alu2fop opc,  6)
  | Alu2fiF(opc,prec,fd,imm) ->
      endpt @@
      (destfreg fd,  6) @@
      (0l,           6) @@
      (mantissa imm, 8) @@
      (bytenum imm,  2) @@
      (precn prec,   2) @@
      (0l,           1) @@
      (1l,           1) @@
      (alu2fop opc,  6)
  | Alu3fF(opc,prec,fd,fm,fn) ->
      endpt @@
      (destfreg fd,  6) @@
      (srcfreg fm,   6) @@
      (srcfreg fn,   6) @@
      (0l,           3) @@
      (precn prec,   2) @@
      (srfexpire fn, 1) @@
      (srfexpire fm, 1) @@
      (0l,           1) @@
      (alu3fop opc,  6)
  | Alu3fiF(opc,prec,fd,fm,imm) ->
      endpt @@
      (destfreg fd,  6) @@
      (srcfreg fm,   6) @@
      (mantissa imm, 8) @@
      (bytenum imm,  2) @@
      (precn prec,   2) @@
      (srfexpire fm, 1) @@
      (1l,           1) @@
      (alu3fop opc,  6)
  | MvcF(rd,imm,fill,repl,part) ->
      endpt @@
      (destreg rd,   6) @@
      (immshift part imm, 16) @@
      (0l,           1) @@
      (fillbit fill, 1) @@
      (replbit repl, 1) @@
      (partbit part, 1) @@
      (20l,          6)
  | CmpF(cond,rd,rm,rn) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rm,    6) @@
      (srcreg rn,    6) @@
      (0l,           3) @@
      (srcexpire rn, 1) @@
      (condcc cond,  3) @@
      (srcexpire rm, 1) @@
      (condopc cond, 6)
  | CmpiF(cond,rd,rm,imm) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rm,    6) @@
      (mantissa imm, 8) @@
      (bytenum imm,  2) @@
      (condcc cond,  3) @@
      (srcexpire rm, 1) @@
      (cndiopc cond, 6)
  | BfxF(rd,rm,lo,hi,ext) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rm,    6) @@
      (!!lo,         5) @@
      (!!hi,         5) @@
      (0l,           1) @@
      (sext ext,     1) @@
      (0l,           1) @@
      (srcexpire rm, 1) @@
      (25l,          6)
  | LdrF(rd,rb,ri,vol,w) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rb,    6) @@
      (srcreg ri,    6) @@
      (srcexpire ri, 1) @@
      (0l,           1) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (width w,      2) @@
      (0l,           2) @@
      (26l,          6)
  | LdriF(rd,rb,off,vol,w) ->
      endpt @@
      (destreg rd,   6) @@
      (srcreg rb,    6) @@
      (offset w off, 8) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (width w,      2) @@
      (0l,           1) @@
      (1l,           1) @@
      (26l,          6)
  | StrF(rs,rb,ri,vol,w) ->
      endpt @@
      (srcreg rs,    6) @@
      (srcreg rb,    6) @@
      (srcreg ri,    6) @@
      (srcexpire ri, 1) @@
      (0l,           1) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (width w,      2) @@
      (srcexpire rs, 1) @@
      (0l,           1) @@
      (27l,          6)
  | StriF(rs,rb,off,vol,w) ->
      endpt @@
      (srcreg rs,    6) @@
      (srcreg rb,    6) @@
      (offset w off, 8) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (width w,      2) @@
      (srcexpire rs, 1) @@
      (1l,           1) @@
      (27l,          6)
  | LdfF(fd,rb,ri,vol,prec) ->
      endpt @@
      (destfreg fd,  6) @@
      (srcreg rb,    6) @@
      (srcreg ri,    6) @@
      (srcexpire ri, 1) @@
      (0l,           1) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (precn prec,   2) @@
      (0l,           2) @@
      (42l,          6)
  | LdfiF(fd,rb,off,vol,prec) ->
      endpt @@
      (destfreg fd,  6) @@
      (srcreg rb,    6) @@
      (offset Word off, 8) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (precn prec,   2) @@
      (0l,           1) @@
      (1l,           1) @@
      (42l,          6)
  | StfF(fs,rb,ri,vol,prec) ->
      endpt @@
      (srcfreg fs,   6) @@
      (srcreg rb,    6) @@
      (srcreg ri,    6) @@
      (srcexpire ri, 1) @@
      (0l,           1) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (precn prec,   2) @@
      (srfexpire fs, 1) @@
      (0l,           1) @@
      (43l,          6)
  | StfiF(fs,rb,off,vol,prec) ->
      endpt @@
      (srcfreg fs,   6) @@
      (srcreg rb,    6) @@
      (offset Word off, 8) @@
      (srcexpire rb, 1) @@
      (volatile vol, 1) @@
      (precn prec,   2) @@
      (srfexpire fs, 1) @@
      (1l,           1) @@
      (43l,          6)
  | LdmF(rb,dra,up,wb) ->
      endpt @@
      (mdreg dra 0,  6) @@
      (mdreg dra 1,  6) @@
      (mdreg dra 2,  6) @@
      (dreg3 rb,     3) @@
      (dir up,       1) @@
      (writebk wb,   1) @@
      (0l,           3) @@
      (28l,          6)
  | StmF(rb,sra,up,wb) ->
      endpt @@
      (mreg sra 0,   6) @@
      (mreg sra 1,   6) @@
      (mreg sra 2,   6) @@
      (dreg3 rb,     3) @@
      (dir up,       1) @@
      (writebk wb,   1) @@
      (mexp sra 0,   1) @@
      (mexp sra 1,   1) @@
      (mexp sra 2,   1) @@
      (29l,          6)
  | LdmfF(rb,dra,up,wb) ->
      endpt @@
      (mdfreg dra 0, 6) @@
      (mdfreg dra 1, 6) @@
      (mdfreg dra 2, 6) @@
      (dreg3 rb,     3) @@
      (dir up,       1) @@
      (writebk wb,   1) @@
      (0l,           3) @@
      (44l,          6)
  | StmfF(rb,sra,up,wb) ->
      endpt @@
      (mfreg sra 0,  6) @@
      (mfreg sra 1,  6) @@
      (mfreg sra 2,  6) @@
      (dreg3 rb,     3) @@
      (dir up,       1) @@
      (writebk wb,   1) @@
      (mfexp sra 0,  1) @@
      (mfexp sra 1,  1) @@
      (mfexp sra 2,  1) @@
      (45l,          6)
  | LdtF(opc,imm) ->
      endpt @@
      (imm,          26) @@
      (ldtopc opc,   6)
  | CbrF(rc,t,f) ->
      endpt @@
      (srcreg rc,    6) @@
      (immreg t,     6) @@
      (immreg f,     6) @@
      (0l,           3) @@
      (srcexpire rc, 1) @@
      (lastir t,     1) @@
      (lastir f,     1) @@
      (immed t,      1) @@
      (immed f,      1) @@
      (48l,          6)
  | CallF(c,r) ->
      endpt @@
      (0l,           6) @@
      (immreg c,     6) @@
      (immreg r,     6) @@
      (0l,           4) @@
      (lastir c,     1) @@
      (lastir r,     1) @@
      (immed c,      1) @@
      (immed r,      1) @@
      (49l,          6)
  | JumpF(j) ->
      endpt @@
      (0l,           6) @@
      (immreg j,     6) @@
      (0l,           10) @@
      (lastir j,     1) @@
      (0l,           1) @@
      (immed j,      1) @@
      (0l,           1) @@
      (50l,          6)
  | RetF ->
      endpt @@
      (0l,           26) @@
      (51l,          6)
  | _ -> raise (UnimplementedOpcode e)
  in
    res

let encodevertices vtx =
  DynArray.fold_right
    (fun tag enclist ->
      let binsns =
        List.fold_right
          (fun insn acc ->
            match insn.Select.s_asm with
              Ast.Ainsn(parts) ->
                List.fold_right
                  (fun insn acc' -> encode insn :: acc')
                  parts
                  acc
            | _ -> failwith "Not register-allocated insn")
          tag.Block.selected
          []
      and widx =
        match tag.Block.writeidx with
          Some foo -> foo
        | None -> failwith "Nowhere to put this block"
      in
        { writeidx = widx; binary = binsns } :: enclist)
    vtx
    []

let showencoded enclist =
  List.iter
    (fun enc ->
      let { writeidx = w; binary = b } = enc in
      Printf.printf "write @ %ld:\n" w;
      List.iter (fun i -> Printf.printf "%.8lx\n" i) b)
    enclist

open Ast

let rec writebinop opc a b =
  let opname =
    match opc with
      Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "div"
    | Lsl -> "lsl"
    | Lsr -> "lsr"
    | Asr -> "asr"
    | Ror -> "ror"
    | And -> "and"
    | Ior -> "ior"
    | Eor -> "eor"
    | Cmp(n) -> writecmp n
    | Udiv -> "udiv"
    | Mod -> "mod"
    | Umod -> "umod"
    | Call -> "call"
    | Add_f p -> writeprec "add_f" p
    | Sub_f p -> writeprec "sub_f" p
    | Mul_f p -> writeprec "mul_f" p
    | Div_f p -> writeprec "div_f" p
    | Trap t -> writetrap t
    | Umlh -> "umlh"
    | Mlh -> "mlh"
  in
    Printf.sprintf "(%s %s %s)" opname (writeop a) (writeop b)

and writecond = function
    Iformat.Eq -> "eq"
  | Iformat.Ne -> "ne"
  | Iformat.Ge -> "ge"
  | Iformat.Geu -> "geu"
  | Iformat.Gt -> "gt"
  | Iformat.Gtu -> "gtu"
  | Iformat.Le -> "le"
  | Iformat.Leu -> "leu"
  | Iformat.Lt -> "lt"
  | Iformat.Ltu -> "ltu"
  | Iformat.Andl -> "and"
  | Iformat.Eorl -> "eor"
  | Iformat.Nandl -> "nand"
  | Iformat.Neorl -> "neor"

and writetcond = function
    Iformat.OnZero -> "z"
  | Iformat.OnNonzero -> "n"

and writecmp x = "cmp/" ^ writecond x

and writetrap t = "trap/" ^ writetcond t

(* and writevecset dir upd =
  let dirname = match dir with
      Inc -> "i"
    | Dec -> "d"
  and whenname = match upd with
      Before -> "b"
    | After -> "a"
  in
    "vecset/" ^ dirname ^ whenname *)

and writeunop opc a =
  let opname =
    match opc with
      Not -> "not"
    | Ind(sz) -> writeind "" sz
    | IndVol(sz) -> writeind "/vol" sz
    | Swi -> "swi"
    | Jump -> "jump"
    | Cast_f p -> writeprec "cast_f" p
    | Neg_f p -> writeprec "neg_f" p
    | Abs_f p -> writeprec "abs_f" p
    | Sqr_f p -> writeprec "sqr_f" p
  in
    Printf.sprintf "(%s %s)" opname (writeop a)

and writenop opc =
  let opname =
    match opc with
      Return -> "return"
  in
    Printf.sprintf "(%s)" opname

and writetriop opc a b c =
  let opname =
    match opc with
      Bitfield -> "ubitf"
    | SignedBitfield -> "sbitf"
    | Splice -> "splice"
    | SpliceShift -> "shiftsplice"
    | Branch -> "branch"
  in
    Printf.sprintf "(%s %s %s %s)" opname (writeop a) (writeop b) (writeop c)

and writeind v sz =
  let szname =
    match sz with
      Byte -> "byte"
    | Halfword -> "halfword"
    | Word -> "word"
    | Doubleword -> "dword"
    | WordVec -> "wordvec"
    | DoublewordVec -> "dwordvec"
  in
    Printf.sprintf "%s%s" szname v

and writebitfield opn s e =
  Printf.sprintf "%s<%d,%d>" opn s e

and writeexpi = function
    LastUse -> "~"
  | Use -> ""
  | Assign -> "="
  | FixedAssign -> "=+"

and writereg typ n sub expi =
  Printf.sprintf "%s%s%d%s" (writeexpi expi) typ n (writeloc sub)

and writepseudo typ n expi =
  Printf.sprintf "%s%s%d" (writeexpi expi) typ n

and writevec vec =
  let rec writevec_short = function
    [] -> ""
  | b::cs -> (writeop b) ^ " " ^ (writevec_short cs)
  in let writevec_long = function
    [] -> ""
  | b::cs -> (writeop b) ^ " ... " ^ (writeop (List.hd (List.rev cs)))
  in let len = List.length vec in
  if len >= 3 then
    writevec_long vec
  else
    writevec_short vec

and writearr ar =
  let buf,_ = Array.fold_left
    (fun (buf,num) node ->
      Buffer.add_string buf (Printf.sprintf "%d:%s " num (writeop node));
      buf, num+1)
    ((Buffer.create 5), 0)
    ar
  in
    Buffer.contents buf

and writeprec n p =
  match p with
    Iformat.Single -> Printf.sprintf "%s(single)" n
  | Iformat.Double -> Printf.sprintf "%s(double)" n

and writenote = function
    ExpireHere eh ->
      let s = Sets.IntSet.fold (fun e s -> Printf.sprintf "%sr%d " s e) 
                               eh.exp_ints
                               ""
      in Printf.sprintf "expirehere { %s}" s

and writeop item =
  match item with
    Zop(opc) -> writenop opc
  | Unop(opc,a) -> writeunop opc a
  | Binop(opc,a,b) -> writebinop opc a b
  | Triop(opc,a,b,c) -> writetriop opc a b c
  | Move(a,b) -> Printf.sprintf "(move %s %s)" (writeop a) (writeop b)
  | Constant(imm) -> "#" ^ (Int32.to_string imm)
  | Register((n,sub), Id.IntType, expi) -> writereg "r" n sub expi
  | Register((n,sub), Id.PtrType, expi) -> writereg "p" n sub expi
  | Register((n,sub), Id.FloatType, expi) -> writereg "f" n sub expi
  | Pseudo(n, Id.IntType, expi) -> writepseudo "ip" n expi
  | Pseudo(n, Id.PtrType, expi) -> writepseudo "pp" n expi
  | Pseudo(n, Id.FloatType, expi) -> writepseudo "fp" n expi
  | Floatconst(imm) -> "#" ^ (string_of_float imm)
(*  | Pseudo(n,ast) -> Printf.sprintf "$%d %s" n (writeop ast) *)
  | Treg(ldt) -> writetreg ldt
  | Note(n,a) -> Printf.sprintf "(*%s %s)" (writenote n) (writeop a)
  | Phi(idl) -> Printf.sprintf "(phi [ %s])" (writearr idl)
  | Syncregs -> Printf.sprintf "(sync_regs)"
  | Reduction { reduction = pt; ruleno = num } ->
      Printf.sprintf "(reduction %d -> %s)" num (writeop pt)
  | Null -> Printf.sprintf "(null)"

and writetreg = function
    X -> "ld/x"
  | Y -> "ld/y"
  | Z -> "ld/z"
  
and writeloc = function
    Id.Unset -> ""
  | Id.Suf(n) -> "[" ^ string_of_int n ^ "]"
  
let rec writectrl = function
    Block.CondBranch(prob, cond, trublk, falblk) ->
      Printf.sprintf "== cbr(%f) %s,%s,%s ==" prob (writeop cond)
                                          (writedest trublk)
                                          (writedest falblk)
  | Block.Call(callblk, retblk) ->
      Printf.sprintf "== call %s,%s ==" (writedest callblk) (writedest retblk)
  | Block.Jump(destblk) ->
      Printf.sprintf "== jump %s ==" (writedest destblk)
  | Block.Return ->
      Printf.sprintf "== ret =="

and writedest = function
    Block.Indirect(dest) -> writeop dest
  | Block.Direct(n) -> "#" ^ (Int32.to_string n)
  | Block.PartialDirect(n) -> "#" ^ (Int32.to_string n) ^ "..."
  | Block.Local(tag) -> "local#" ^ (string_of_int tag.Block.dfnum)

let writeinsns insns =
  List.iter (fun n -> print_endline (writeop n)) (List.rev insns)

let writeblk blk =
  let Block.Block(insns,term) = blk in
    writeinsns insns;
    print_endline (writectrl term)

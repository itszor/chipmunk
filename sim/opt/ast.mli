
(*type reg_note = DefinitionKilled of id
              | UsageFolded of id
              | Defined of id
              | Used of id*)

type ast =
    Triop of op3 * ast * ast * ast
  | Binop of op2 * ast * ast
  | Unop of op1 * ast
  | Zop of op0
  | Move of ast * ast
  | Register of reg
  | Pseudo of pseudo
  | Constant of int32
  | Floatconst of float
  | Treg of ldtype
  | Note of note * ast
  | Phi of ast array
  | Syncregs
  | Reduction of red
  | Null

and red = {
  reduction : ast;
  ruleno : int;
  cost : int;
  cost_with_kids : int;
  defs : ast list;
  uses : ast list;
  factored_vars : ast list;
  insn : insnorphi
}

and insnorphi =
    Insn of (Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Iformat.format list)
  | Ainsn of Iformat.format list
  | Iphi of ast * ast array

and reg = Id.id * Id.regtype * usage
and pseudo = int * Id.regtype * usage
and usage = Assign | Use | LastUse | FixedAssign
and size = Byte | Halfword | Word | Doubleword | WordVec | DoublewordVec
and ldtype = X | Y | Z
and op0 = Return
and op1 = Not
        | Ind of size
        | IndVol of size
        | Swi
        | Jump
        | Neg_f of Iformat.precision
        | Abs_f of Iformat.precision
        | Sqr_f of Iformat.precision
        | Cast_f of Iformat.precision
and op2 =
    Add
  | Sub
  | Mul
  | Div
  | Lsl
  | Lsr
  | Asr
  | Ror
  | And
  | Ior
  | Eor
  | Cmp of Iformat.cond
  | Udiv
  | Mod
  | Umod
  | Mlh
  | Umlh
  | Call
  | Add_f of Iformat.precision
  | Sub_f of Iformat.precision
  | Mul_f of Iformat.precision
  | Div_f of Iformat.precision
  | Trap of Iformat.tcond
and op3 = Branch
        | Bitfield
        | SignedBitfield
        | SpliceShift
        | Splice

and note = ExpireHere of regset

and regset = {
  mutable exp_ints : Sets.IntSet.t;
  mutable exp_flts : Sets.IntSet.t
}

module AstSet : Set.S with type elt = ast
module AstMap : Map.S with type key = ast

(*and reginfo = {
  mutable note : Sets.RegnoteSet.t;
  mutable avail : Sets.IntSet.t
}*)

exception AstNotRegisterLike of ast

val ast_of_reg_or_pseudo : Id.reg_or_pseudo -> usage -> ast
val reg_or_pseudo_of_ast : ast -> Id.reg_or_pseudo

val is_register : ast -> bool

val iter : (ast -> unit) -> ast -> unit
val fold_ltor_postorder : (ast -> 'a -> 'a) -> ast -> 'a -> 'a
val fold_rtol_postorder : (ast -> 'a -> 'a) -> ast -> 'a -> 'a

val list_fold_forwards : ('a -> ast -> 'a) -> 'a -> ast list -> 'a
val list_fold_backwards : (ast -> 'a -> 'a) -> ast list -> 'a -> 'a

val map_postorder : (ast -> ast) -> ast -> ast
val seq_of_ast_rl : ast -> ast Seq.seq
val seq_of_ast_lr : ast -> ast Seq.seq
val ast_of_seq_rl : ast Seq.seq -> ast
val ast_of_seq_lr : ast Seq.seq -> ast

(* val makemakepseudo : unit -> 'a -> Ast.ast
val newpseudo : unit -> Ast.ast *)

exception Fail

type env = (string, Ast.ast) Hashtbl.t

type ccr = {
  m_cost : int;
  m_children : Memotree.memo list;
  m_reduction : Ast.ast Memotree.matchtype;
  m_ast : Ast.ast;
}

type matcher = env -> Ast.ast -> Memotree.memotree -> ccr -> ccr

val nonmatched : Memotree.memotree -> Memotree.memotree

val denoted : Ast.ast -> Ast.ast

(* val match_reg : string -> matcher
val match_freg : string -> matcher
val match_const : int32 -> matcher *)
val match_shiftimm : Ast.ast -> bool
val match_dataimm : Ast.ast -> bool
val match_negdataimm : Ast.ast -> bool
val match_maybenegdataimm : Ast.ast -> bool
val negate_ast_const : Ast.ast -> Ast.ast
val match_floatimm : Ast.ast -> bool
val match_invdataimm : Ast.ast -> bool
val match_ehimm :Ast.ast -> bool
val match_elimm : Ast.ast -> bool
val match_fhimm : Ast.ast -> bool
val match_flimm : Ast.ast -> bool
val match_wmimm : Ast.ast -> bool
val match_hmimm : Ast.ast -> bool
val match_bmimm : Ast.ast -> bool

(*
val match_imm : int32 -> bool
val match_triop : Ast.op3 -> (matcher) -> (matcher) -> (matcher) -> matcher
val match_binop : Ast.op2 -> (matcher) -> (matcher) -> matcher
val match_unop : Ast.op1 -> (matcher) -> matcher
val match_nop : Ast.op0 -> matcher
val match_ind : Ast.size -> string -> (matcher) -> matcher
val match_bitpair : string -> string -> matcher
*)

(*
exception EmptyList

val getstartend : 'a list -> 'a * 'a
val match_vector : string -> string -> matcher
(* val match_vectorf : string -> string -> matcher *)
val match_move : (matcher) -> (matcher) -> matcher
val match_reduction : (env -> Ast.ast) -> (matcher) -> matcher
val make_temp : string -> env -> Ast.ast
*)

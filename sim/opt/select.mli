exception NoMatches

exception SelectFailed of Ast.ast

type selectedast = {
  mutable s_asm: Ast.insnorphi;
  s_def: Id.reg_or_pseudo list;
  s_use: Id.reg_or_pseudo list;
  mutable s_live_in: Sets.RegOrPseudoSet.t;
  mutable s_live_out: Sets.RegOrPseudoSet.t;
  s_num: int
}

type selectedinsns = Ast.ast * Ast.ast list

val select : Ast.ast -> selectedinsns

val accum_selected_insns : selectedinsns -> selectedast list -> int ->
                           selectedast list * int

val accum_insns_for_ast : Ast.ast -> selectedast list -> int ->
                          selectedast list * int

val insns_for_ast : Ast.ast -> int -> selectedast list

(*
val select : (int *
             (('a, 'b) Hashtbl.t -> Ast.ast -> Memotree.memotree ->
               Mtree.ccr -> Mtree.ccr) *
             (('a, 'b) Hashtbl.t -> Iformat.format list)) list ->
             Ast.ast -> Memotree.memo

val split : Memotree.memo -> selectedast list
*)

(*
val emit : Ast.ast -> Memotree.memotree -> Iformat.format list

val select :
  (int * ((string, Ast.mutablenode) Hashtbl.t ->
    Ast.memoized -> Ast.mutablenode option * Ast.memoized list) *
   (Ast.environ -> Ast.ilist)) list ->
  Ast.memoized -> Iformat.format list

val dumpenv : Ast.memoized -> unit
*)

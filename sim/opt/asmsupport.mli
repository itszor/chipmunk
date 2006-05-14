val cost_int_node : Ast.ast -> int
val cost_float_node : Ast.ast -> int

val kids_int_node : Ast.ast -> Ast.ast list -> Ast.ast list
val kids_float_node : Ast.ast -> Ast.ast list -> Ast.ast list

val get_const : Ast.ast -> int32
val get_num : Ast.ast -> int
val split_lobits : Ast.ast -> int32
val split_hibits : Ast.ast -> int

val lookup_destreg : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Ast.ast ->
                     Iformat.destreg
val lookup_srcreg : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Ast.ast ->
                    Iformat.srcreg
val lookup_destfreg : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Ast.ast ->
                      Iformat.destfreg
val lookup_srcfreg : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Ast.ast ->
                     Iformat.srcfreg

val gen_int_pseudo : unit -> Ast.ast

exception FailedAlloc of Ast.ast

(*exception NotFound of string
exception TypeMismatch of string

type ilist = Iformat.format list

and environ = (string, Ast.ast) Hashtbl.t

val lookup_ast : (string, 'a) Hashtbl.t -> string -> 'a
val lookup_destreg :
  (string, Ast.ast) Hashtbl.t -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> 
  string -> Iformat.destreg
val lookup_srcreg :
  (string, Ast.ast) Hashtbl.t -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
  string -> Iformat.srcreg
val lookup_destfreg :
  (string, Ast.ast) Hashtbl.t -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
  string -> Iformat.destfreg
val lookup_srcfreg :
  (string, Ast.ast) Hashtbl.t -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
  string -> Iformat.srcfreg

val lookup_vol : (string, Ast.ast) Hashtbl.t -> string -> Iformat.volatile
val lookup_imm : (string, Ast.ast) Hashtbl.t -> string -> int32
val lookup_num : (string, Ast.ast) Hashtbl.t -> string -> int
val lookup_hibits : (string, Ast.ast) Hashtbl.t -> string -> int
val lookup_lobits : (string, Ast.ast) Hashtbl.t -> string -> int32
*)

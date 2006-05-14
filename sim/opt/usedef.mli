type usedef = {
  mutable def : (Ast.ast * Block.tag) option;
  mutable stmts_use : (Ast.ast * Block.tag) list;
  mutable use : Ast.ast list
}

val count : Block.tag DynArray.t -> usedef Sets.RegOrPseudoMap.t
val ast_uses : Ast.ast -> Ast.ast list
val count_stmts_use : usedef Sets.RegOrPseudoMap.t -> unit

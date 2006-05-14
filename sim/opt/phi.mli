val make_root : Block.tag DynArray.t -> int
val defsites : Block.tag DynArray.t -> int ->
               Sets.IntSet.t Sets.RegOrPseudoMap.t
val place : Block.tag DynArray.t -> Sets.IntSet.t Sets.RegOrPseudoMap.t -> unit

(*
val rewrite_uses : Ast.ast list -> (Id.reg_or_pseudo, BitSet.t) PMap.t ->
                   Ast.ast list
val rewrite_defs : Ast.ast list -> (Id.reg_or_pseudo, BitSet.t) PMap.t ->
                   Ast.ast list
val rewrite_statements : Block.block -> (Id.reg_or_pseudo, BitSet.t) PMap.t ->
                         Block.block
*)

val rename : Block.tag -> Sets.IntSet.t Sets.RegOrPseudoMap.t -> unit
val eliminate : Block.tag DynArray.t -> unit

type terminate =
    CondBranch of float * Ast.ast * blockref * blockref
  | Call of blockref * blockref
  | Jump of blockref
  | Return

and blockref =
    Indirect of Ast.ast
  | Direct of int32
  | PartialDirect of int32
  | Local of tag
 (* | Self of int32 *)

and tag = {
  mutable domfront : tag list;
  mutable parent : tag option;
  mutable predecessors : tag list;
  mutable successors : tag list;
  mutable semi : tag option;
  mutable idom : tag option;
  mutable idomchild : tag list;
  mutable ancestor : tag option;
  mutable samedom : tag option;
  mutable bucket : tag list;
  mutable dfnum : int;
  mutable block : block;
  mutable selected : Select.selectedast list;
  mutable refno : int32;
  mutable writeidx : int32 option;
  mutable live_in : Sets.RegOrPseudoSet.t;
  mutable live_out : Sets.RegOrPseudoSet.t
}

and colour = White | Grey | Black

and block = Block of Ast.ast list * terminate

val termsize : terminate -> int
val blocksize : block -> int

val block_fold_forwards : ('a -> Ast.ast -> 'a) -> 'a -> block -> 'a
val block_fold_backwards : (Ast.ast -> 'a -> 'a) -> block -> 'a -> 'a
val map_postorder : (Ast.ast -> Ast.ast) -> block -> block

val apply_term_as_ast_forwards : ('a -> Ast.ast -> 'a) -> 'a -> terminate -> 'a
val apply_term_as_ast_backwards : (Ast.ast -> 'a -> 'a) -> terminate -> 'a -> 'a

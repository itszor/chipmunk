type memo = {
  reduce : Ast.ast matchtype;
  astequiv: Ast.ast;
  cost : int;
  asm : asmbinding;
  children : memo list
}

and memotree = MTri of memotree * memotree * memotree
             | MBin of memotree * memotree
             | MUn of memotree
             | MNull
             | MMatch of memo * memotree
             | MVec of memotree list

and asmbinding = (string, Ast.ast) Hashtbl.t ->
  Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Iformat.format list

and 'a matchtype = IntNode of 'a | FloatNode of 'a | NoMatch

val string_of_memotree : memotree -> string
val iter : (memotree -> unit) -> memotree -> unit
val map : (memotree -> memotree) -> memotree -> memotree
val fold_left : ('a -> memotree -> 'a) -> 'a -> memotree -> 'a
val fold_right : (memotree -> 'a -> 'a) -> memotree -> 'a -> 'a

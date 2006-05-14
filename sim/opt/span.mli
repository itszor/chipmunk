type span = {
  born: int option;
  dies: int option;
  usedat: int list;
  broken: bool;
}

val findspans : Ast.ast list -> span Sets.SpanMap.t
val breakspans : Ast.ast list -> span Sets.SpanMap.t -> span Sets.SpanMap.t
val printspans : span Sets.SpanMap.t -> unit
val substitute_pseudos : Ast.ast list -> span Sets.SpanMap.t ->
                         Ast.ast list

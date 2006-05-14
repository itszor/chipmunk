(*type rangetype = Term of int * int
               | Top of Id.id * int * int
               | Bottom of Id.id * int * int

type partinfo = Born of Id.id * int
              | Dies of Id.id * int*)

val subblocks : Ast.ast list -> (int * int) list
val printsubblocks : (int * int) list -> unit
val definitelydead : (int * int) list -> Span.span Sets.SpanMap.t ->
                     (int * Sets.IntSet.t) list
val printdefdead : (int * Sets.IntSet.t) list -> unit
val rewrite : Ast.ast list -> (int * Sets.IntSet.t) list -> Ast.ast list

(*
val partial : Span.span Sets.SpanMap.t -> partinfo list
val print : int list -> unit
val splitblocks : Ast.ast list -> Span.span Sets.SpanMap.t -> rangetype list
val printsubblocks : rangetype list -> unit
val recover : Ast.ast list -> Span.span Sets.SpanMap.t -> rangetype list ->
  (int * Sets.IntSet.t) list
val printrecovered : (int * Sets.IntSet.t) list -> unit
*)

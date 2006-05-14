(* Exported interface for encode *)

type encoded = {
  writeidx : int32;
  binary : int32 list
}

val encode : Iformat.format -> int32
val encodevertices : Block.tag DynArray.t -> encoded list
val showencoded : encoded list -> unit

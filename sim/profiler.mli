exception NotOptimised

class profiler :
  object
    val hash : (int32 * int32, int ref) Hashtbl.t
    method count : int32 -> int32 -> bool
    method dumprecur : unit
    method count_cbr : int32 -> Ast.ldtype -> unit
    method get_cbr : int32 -> float
    method getrep : int32 -> int32 -> int
  end

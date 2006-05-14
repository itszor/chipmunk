val directmap : int -> int -> int32 -> int list
val nway : int -> int -> int -> int32 -> int list
val twoway : int -> int -> int32 -> int list
val fourway : int -> int -> int32 -> int list
val eightway : int -> int -> int32 -> int list
exception Not_in_cache
exception Assoc_full
class cache :
  < gettime : int64; .. > ->
  int ->
  int ->
  (int32 -> int list) ->
  object
    val cache :
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
    val idx :
      (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
    val mutable latency : int
    method baseaddr : int32 -> int32
    method fill : int -> int32 array -> int64 * (unit -> unit)
    method flush : unit
    method idxaddr : int32 -> int32
    method incache : int32 -> bool
    method readline : int -> int32 array
    method try_allocate : int32 -> int
    method writeline : int -> int32 array -> unit
  end

type entry = Empty | Counter of int32 * int32 * int ref | Saturated
exception SomeMistake
exception Trigger of int32 * int32
val defaulthash : int -> int32 -> int32 -> int
class tcache :
  int ->
  (int32 -> int32 -> int) ->
  object
    val mutable cache : entry ref array
    val mutable trigger : int
    method private first : entry ref -> int32 -> int32 -> unit
    method private inc : entry ref -> int32 -> int32 -> unit
    method reset : unit
    method settrigger : int -> unit
    method trans : int32 -> int32 -> unit
    method values : entry ref array
  end

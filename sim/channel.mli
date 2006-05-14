type wrapper =
    WrapInt of int
  | WrapString of string
  | WrapIntList of int list
  | WrapInt32 of int32
  | WrapInt32List of int32 list

exception TypeError

class channel :
  in_channel ->
  out_channel ->
  object
    val mutable currentid : int
    val funs : (string, wrapper -> wrapper) Hashtbl.t
    val ic : in_channel
    val inprogress : Mutex.t
    val oc : out_channel
    method call : string -> wrapper -> wrapper
    method listen : bool -> unit
    method makeid : int
    method makelocal : string -> (wrapper -> wrapper) -> unit
    method waitforinput : unit
  end

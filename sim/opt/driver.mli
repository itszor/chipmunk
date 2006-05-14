val writelist :
  < writeWord : int32 -> 'a -> 'b; .. > -> int32 -> 'a list -> unit
(*val common_transition :
  State.machine -> int32 -> int32 -> Tag.tag option * int32 array * int32 list*)

(* Only temporary... *)
val common_transition : State.machine -> int32 -> int32 -> unit

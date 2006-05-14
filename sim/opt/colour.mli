(*
val alloc : Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
  Usedef.usedef Sets.RegOrPseudoMap.t -> Interference.intfmap ->
  Id.reg_or_pseudo Sets.RegOrPseudoMap.t
*)

val alloc : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> Sets.IntfGraph.t ->
  Regpool.t -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t * Id.reg_or_pseudo list

val write_alloc : out_channel -> Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
  Id.reg_or_pseudo list -> Sets.IntfGraph.t -> unit

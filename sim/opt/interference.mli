type intfmap = Sets.RegOrPseudoSet.t Sets.RegOrPseudoMap.t

val liveness_analysis : Usedef.usedef Sets.RegOrPseudoMap.t -> intfmap
val interferes : intfmap -> Id.reg_or_pseudo -> Id.reg_or_pseudo -> bool

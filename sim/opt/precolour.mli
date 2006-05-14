val find_fixed_assignments : Block.tag DynArray.t ->
                             Id.reg_or_pseudo Sets.RegOrPseudoMap.t
val write_fixed_assignments : Id.reg_or_pseudo Sets.RegOrPseudoMap.t -> unit

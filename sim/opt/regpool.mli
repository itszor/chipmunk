type t = {
  intregs : Sets.RegOrPseudoSet.t;
  ptrregs : Sets.RegOrPseudoSet.t;
  floatregs : Sets.RegOrPseudoSet.t
}

val initial_pools : t

exception Spill

val mark_allocated : Id.reg_or_pseudo -> Sets.RegOrPseudoSet.t ->
                     Sets.RegOrPseudoSet.t

val choose_reg : Id.reg_or_pseudo Sets.RegOrPseudoMap.t ->
                 Sets.RegOrPseudoSet.t -> Sets.RegOrPseudoSet.t ->
                 Id.reg_or_pseudo -> t -> Id.reg_or_pseudo

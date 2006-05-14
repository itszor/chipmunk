type tag = Tag_in_probation
         | Tag_never_optimise
         | Tag_trap_converted
         | Tag_jump_folded
         | Tag_specialised_data_profile
         | Tag_canonical_name
         | Tag_timing_instrumented

val put_metadata : State.machine -> int32 -> tag -> int32 array -> unit
val get_metadata : State.machine -> int32 -> (tag * int32 array)
val blockname : State.machine -> int32 -> string

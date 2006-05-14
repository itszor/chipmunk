external load_elf : string -> int32 = "load_elf_c"
val idxbase : int32
val textbase : int32
val database : int32
val rodatabase : int32
val namebase : int32
val bssbase : int32
val lastidx : int32
val clearbss : State.machine -> unit

val makecanonicalnames : State.machine -> int32 -> unit

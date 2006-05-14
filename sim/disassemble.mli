exception BadFormat
val vmputstring : State.machine -> int32 -> string -> unit
val vmgetstring : State.machine -> int32 -> string
val diss : State.machine -> Iformat.format -> string
val dissblock : out_channel -> State.machine -> int32 -> unit

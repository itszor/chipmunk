val shift : State.machine -> unit
val nop : Iformat.format
val currentinst :
  < getdlatch : Iformat.format -> Iformat.format; .. > -> Iformat.format
val currentrawinst :
  < readWord : int32 -> 'a; .. > -> < getpc : int32; .. > -> 'a
val run : State.machine -> unit
val run_with_trace : State.machine -> unit
